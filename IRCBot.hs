{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.STM.TVar
import Control.Exception
import Control.Monad
import Control.Monad.State
import Control.Monad.STM
import Control.Monad.Trans.Reader
import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Tuple
import Network
import System.IO
import Text.Printf

-- types
type Str = String

data MessagePrefix = MessagePrefix {
    mNick :: Str,
    mUser :: Str,
    mHost :: Str
  } deriving(Show)

data Message = Message {
    mPrefix :: Maybe MessagePrefix,
    mCommand :: Str,
    mMiddle :: [Str],
    mTrailing :: Str,
    mRawData :: Str
  } deriving(Show)

-- parsing
shift :: ((Str -> Str) -> (Str, Str)) -> Char -> State Str Str
shift strategy char = do
    (h, t) <- gets $ shift' char id
    put t
    return h
  where
    shift' _ acc [] = strategy acc
    shift' char acc (x:xs)
      | char == x = (acc [], xs)
      | otherwise = shift' char (acc . (x:)) xs

rollbackShift acc = ([], acc [])
commitShift = swap . rollbackShift

parse :: Str -> Message
parse = evalState getMessage where
  getMessage = do
    h <- gets head
    prefix <- if h == ':'
              then shift commitShift ' ' >>= return . Just . evalState getPrefix
              else return Nothing
    command <- map toLower <$> shift commitShift ' '
    message <- get
    middle <- split (condense . dropDelims . dropFinalBlank $ oneOf " ") <$> shift commitShift ':'
    trailing <- get
    return $ Message { mPrefix = prefix, mCommand = command, mMiddle = middle, mTrailing = trailing, mRawData = message }
  getPrefix = do
    modify tail
    nick <- shift rollbackShift '!'
    user <- shift rollbackShift '@'
    host <- get
    return $ MessagePrefix { mNick = nick, mUser = user, mHost = host }

-- misc
eqToBy v e = (== v) . e
cons = (:)
f $> v = f <*> pure v
writeTVarIO a b = atomically $ writeTVar a b
both f g v = f v >> g v

bRead = asks >=> lift . readTVar
bWrite var val = asks var >>= lift . flip writeTVar val
bModify var val = asks var >>= lift . flip modifyTVar val

getOrAdd collection predicate defAction = do
  collectionItems <- bRead collection
  let maybeItem = find predicate collectionItems
      addItem = defAction >>= \item -> bWrite collection (item : collectionItems) >> return item
  maybe addItem return maybeItem

-- state types
type UId = Int
newtype UserId = UserId UId deriving(Show, Eq)
newtype ChannelId = ChannelId UId deriving(Show, Eq)

data Authority = Owner | Operator | Helper | Voiced deriving(Show, Eq)
data User = User { uId :: UserId, uName :: Str, uUser :: Maybe Str, uHost :: Maybe Str } deriving(Show)
data Participant = Participant { pUserId :: UserId, pChannelId :: ChannelId, pAuthority :: [Authority] } deriving(Show)
data Channel = Channel { cId :: ChannelId, cName :: Str } deriving(Show)
data Bot = Bot {
  sUser :: UserId,
  sUsers :: TVar [User],
  sParticipants :: TVar [Participant],
  sChannels :: TVar [Channel],
  sNextId :: TVar UId,
  sDoPrintMesage :: TVar Bool
}

-- state management
eqP u c p = pUserId p == uId u && pChannelId p == cId c

getId = do
  nextId <- bRead sNextId
  bWrite sNextId (nextId + 1)
  return nextId

selectFunctionByNick me they nick = do
  user <- asks sUser
  Just u <- find (eqToBy user uId) <$> bRead sUsers
  return $ (if nick == uName u then me else they) nick

quitMe _ = return ()

quitThey nick = do
  ([User { uId = duid }], lul) <- partition (eqToBy nick uName) <$> bRead sUsers
  bWrite sUsers lul
  bModify sParticipants (filter (not . eqToBy duid pUserId))  

joinMe nick _ _ channel = do
  c <- getOrAdd sChannels (eqToBy channel cName) (getId >>= \cid -> return $ Channel { cId = ChannelId cid, cName = channel })
  user <- asks sUser
  Just u <- find (eqToBy user uId) <$> bRead sUsers
  void $ getOrAdd sParticipants (eqP u c) (return $ Participant { pUserId = uId u, pChannelId = cId c, pAuthority = [] })

joinThey nick user host channel = do
  let nu = getId >>= \uid -> return $ User { uId = UserId uid, uName = nick, uUser = user, uHost = host }
  u <- getOrAdd sUsers (eqToBy nick uName) nu
  Just c <- find (eqToBy channel cName) <$> bRead sChannels
  void $ getOrAdd sParticipants
    (eqP u c)
    (return $ Participant { pUserId = uId u, pChannelId = cId c, pAuthority = [] })

partMe _ channel = do
  ([dc], lcl) <- partition (eqToBy channel cName) <$> bRead sChannels
  bWrite sChannels lcl
  (_, lpl) <- partition (eqToBy (cId dc) pChannelId) <$> bRead sParticipants
  bWrite sParticipants lpl
  let luidl = map pUserId lpl
      pr u = not (uId u `elem` luidl)
  (_, lul) <- partition pr <$> bRead sUsers
  bWrite sUsers lul

partThey nick channel = do
  ([du], lul) <- partition (eqToBy nick uName) <$> bRead sUsers
  Just c <- find (eqToBy channel cName) <$> bRead sChannels
  ([_], lpl) <- partition (eqP du c) <$> bRead sParticipants
  bWrite sParticipants lpl
  when (not $ uId du `elem` map pUserId lpl) $ bWrite sUsers lul

nickAny from to = do
  ([cu], lul) <- partition (eqToBy from uName) <$> bRead sUsers
  bWrite sUsers (cu { uName = to } : lul)

updateUser nick user host = do
  ([u@User { uUser = uu, uHost = uh }], ul) <- partition (eqToBy nick uName) <$> bRead sUsers
  when (isNothing uu || isNothing uh) $ bWrite sUsers $ u { uUser = Just user, uHost = Just host } : ul

updateAuthority nick channel authority = do
  Just u <- find (eqToBy nick uName) <$> bRead sUsers
  Just c <- find (eqToBy channel cName) <$> bRead sChannels
  ([p], pl) <- partition (eqP u c) <$> bRead sParticipants
  bWrite sParticipants $ p { pAuthority = authority $ pAuthority p } : pl
  
extractAuthority token = case token of
    '^':token -> progress Owner token
    '@':token -> progress Operator token
    '%':token -> progress Helper token
    '+':token -> progress Voiced token
    nick -> ([], nick)
  where progress auth token = let (auth', nick) = extractAuthority token in (auth:auth', nick)
  

-- reaction
process :: Message -> ReaderT Bot STM [Str]
process (Message { .. }) = let
    ret = return [] :: ReaderT Bot STM [Str]
    inMaybe = const ret <=< fromMaybe (return ())
    sel = join . selectFunctionByNick
  in asks sUser >>= \user -> find (eqToBy user uId) <$> bRead sUsers >>= \(Just mn) -> case mCommand of
    "376" -> return $ if null channelsToJoin then [] else ["join " ++ (concat $ intersperse "," channelsToJoin)]
    "join" -> do
      let (channel : _) = mMiddle
      inMaybe $ do
        nick <- mNick <$> mPrefix
        user <- mUser <$> mPrefix
        host <- mHost <$> mPrefix
        return $ sel joinMe joinThey nick $> Just user $> Just host $> channel
    "part" -> do
      let (channel : _) = mMiddle
      inMaybe $ do
        nick <- mNick <$> mPrefix
        return $ sel partMe partThey nick $> channel
    "kick" -> do
      let (channel : nick : _) = mMiddle
      sel partMe partThey nick $> channel
      ret
    "quit" -> do
      inMaybe $ do
        nick <- mNick <$> mPrefix
        return $ sel quitMe quitThey nick
    "353" -> do
      let (_ : _ : channel : _) = mMiddle
          userList = splitOneOf " " mTrailing
          addUser (auth, nick) = do
            joinThey nick Nothing Nothing channel
            updateAuthority nick channel $ const auth
      mapM_ (addUser . extractAuthority) userList
      ret
    "nick" -> do
      let [newNick] = mMiddle
      inMaybe $ do
        nick <- mNick <$> mPrefix
        return $ nickAny nick newNick
      ret
    "352" -> do
      let (_ : channel : user : host : hostName : nick : ('H' : authority) : _) = mMiddle
      updateUser nick user host
      updateAuthority nick channel $ const $ fst (extractAuthority authority)
      ret
    "mode" -> do
      case mMiddle of
        (channel : [dir, mode] : nick : _) -> do
          let act = case dir of '-' -> delete; '+' -> cons
              auth = case mode of 'o' -> Just Operator; 'h' -> Just Helper; 'v' -> Just Voiced; _ -> Nothing
          fromMaybe (return ()) $ updateAuthority nick channel <$> (act <$> auth)
        _ -> return ()
      ret
    "privmsg" -> do
      let (target : _) = mMiddle
      case mTrailing of
        '!':cmd ->
          case cmd of
            'd':' ':text -> return [text]
            _ -> ret
        _ -> ret
    _ -> ret

serveNet r w p bot = forever $ do
    line <- r
    case parse line of
      (Message { mCommand = "ping", mTrailing = trailing }) -> w $ "PONG " ++ trailing
      message -> do
        pr "I" line
        doPrintMessage <- readTVarIO $ sDoPrintMesage bot
        when doPrintMessage $ pr "IMsg" $ show message
        flip catch (print :: SomeException -> IO ()) $ do
          replies <- atomically $ flip runReaderT bot $ process message
          mapM_ (both w $ pr "O") replies
  where pr pref str = p $ pref ++ " " ++ str

serveConsole r w p bot@(Bot { .. }) = forever $ r >>= \line -> case line of
  '/':cmd -> case flip runState cmd $ shift commitShift ' ' of
    ("msg", tail) -> let (target, msg) = flip runState tail $ shift commitShift ' ' in (both w p) $ "PRIVMSG " ++ target ++ " :" ++ msg
    ("prnmsg", state) -> writeTVarIO sDoPrintMesage (read state)
    ("showstate", []) -> do
      let pv :: (Show a) => TVar a -> IO ()
          pv = readTVarIO >=> p
      pv sUsers
      pv sParticipants
      pv sChannels
      pv sNextId
      pv sDoPrintMesage
    c -> p $ "Unknown command " ++ show c
  _ -> w line

channelsToJoin = ["#cb"]
myDefaultNick = "HaskBot"
myIdent = "X"
server = "chat.freenode.net"
port   = 6667 :: Int

main = do
  c <- newChan
  pc <- newChan
  h <- connectTo server (PortNumber (fromIntegral port))
  let w = writeChan c
      p = writeChan pc
  forkIO $ forever $ readChan c >>= hPrintf h "%s\r\n"
  forkIO $ forever $ readChan pc >>= putStrLn
  hSetBuffering h NoBuffering
  hSetEncoding h utf8
  both w p $ "NICK " ++ myDefaultNick
  both w p $ "USER " ++ myDefaultNick ++ " X * :" ++ myIdent
  let startId = 0
  (ul, pl, cl, lid, pm) <- atomically $ (,,,,)
    <$> newTVar [User { uId = UserId startId, uName = myDefaultNick, uUser = Nothing, uHost = Nothing }]
    <*> newTVar [] <*> newTVar [] <*> newTVar (startId + 1) <*> newTVar False
  let bot = Bot { sUser = UserId startId, sUsers = ul, sParticipants = pl, sChannels = cl, sNextId = lid, sDoPrintMesage = pm }
  forkIO $ serveConsole (delete '\r' <$> getLine) w p bot
  serveNet (delete '\r' <$> hGetLine h) w p bot
