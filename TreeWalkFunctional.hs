import Control.Monad

data Tree v = Node { value :: v, children :: [Tree v] }

main = let
    tree = Node 1 [
      Node 11 [
        Node 111 [
          Node 1111 []
        ],
        Node 112 [
          Node 1121 [],
          Node 1122 []
        ]
      ],
      Node 12 [
        Node 121 [
          Node 1211 [],
          Node 1212 []
        ],
        Node 122 []
      ]]
  in mapM_ (\(m, f) -> print m >> print (f tree)) [
    ("Calling Depth-first forward variant", depthWalk),
    ("Calling Depth-first backward variant", depthBackwardWalk),
    ("Calling Breadth-first variant", breadthWalk)
    ]

depthWalk (Node v c) =
  (v :) $ c >>= depthWalk

depthBackwardWalk (Node v c) =
  (v :) $ reverse c >>= depthBackwardWalk

breadthWalk tree = walk [tree]
  where
    walk [] = []
    walk nodes = map value nodes ++ walk (nodes >>= children)
