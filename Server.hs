import Network
import Data.List
import qualified Data.Text as T
import System.IO
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State.Lazy
import Control.Arrow
import Control.Concurrent
import Control.Concurrent.MVar

data User = User { userHandle :: Handle, userName :: String }

trim = T.unpack . T.strip . T.pack
{-trim = fst . spanEnd e . dropWhile e
  where e = flip elem " \t\n\r"

spanEnd p = join (***) ($ []) . foldl' (\(acc, rej) cur -> if p cur then (acc, rej . (cur :)) else (acc . rej . (cur :), id)) (id, id)
trimEnd p = snd . foldr (\cur (p, acc) -> if p cur then (p, acc) else (const False, cur : acc)) (p, [])-}

sendAll clients str = readMVar clients >>= mapM_ (flip hPutStrLn str . userHandle)

process clients handle = let
    send = liftIO . sendAll clients
    getTrimmedLine = fmap trim $ hGetLine handle
    processMessages = do
      line <- liftIO getTrimmedLine
      name <- get
      case line of
        '/':'w':'h':'o':_ -> liftIO $ readMVar clients >>= hPutStrLn handle . intercalate ", " . map userName
        '/':'m':'e':' ':msg -> send $ " * " ++ name ++ " " ++ msg
        '/':'n':'a':'m':'e':' ':newName -> do
          liftIO $ modifyMVar_ clients $ return . map (\u@(User h n) -> if n == name then User h newName else u)
          send $ " ** <" ++ name ++ "> has changed name to <" ++ newName ++ ">"
          put newName
        msg -> send $ "<" ++ name ++ "> " ++ msg
      processMessages
  in do
    hPutStrLn handle "Please enter you name:"
    name <- getTrimmedLine
    hPutStrLn handle $ "Hello, " ++ name
    sendAll clients $ " ** <" ++ name ++ "> has joined"
    modifyMVar_ clients $ return . (User handle name :)
    execStateT processMessages name

main = withSocketsDo $ do
  lsock <- listenOn $ PortNumber 6667
  clients <- newMVar []
  forever $ do
    (handle, _, _) <- accept lsock
    forkFinally (process clients handle) (\_ ->
      modifyMVar clients (return . partition ((/= handle) . userHandle))
        >>= mapM_ (\(User _ name) -> sendAll clients $ " ** <" ++ name ++ "> has quit"))
