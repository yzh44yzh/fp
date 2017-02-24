{-# OPTIONS_GHC -Wall #-}

module LogAnalisys where

import Control.Applicative

data MessageType = Info | Warning | Error Int deriving (Show, Eq)

type TimeStamp = Int

data LogMessage = LogMessage MessageType TimeStamp String | Unknown String deriving (Show, Eq)

data MessageTree = Leaf | Node MessageTree LogMessage MessageTree deriving (Show, Eq)


parseMessage :: String -> LogMessage
parseMessage str =
    case words str of
      "E" : level : timestamp : rest -> lm (Error (read level)) timestamp rest
      "W" : timestamp : rest -> lm Warning timestamp rest
      "I" : timestamp : rest -> lm Info timestamp rest
      _ -> Unknown str
    where lm = \msgType timestamp ws ->
               LogMessage msgType (read timestamp) (unwords ws)


parse :: String -> [LogMessage]
parse str = map parseMessage (lines str)


insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert myLog tree =
    case tree of
      Leaf -> Node Leaf myLog Leaf
      Node left currLog right ->
          if less myLog currLog then Node (insert myLog left) currLog right
          else Node left currLog (insert myLog right)
    where
      less = \log1 log2 ->
             let
                 LogMessage _ ts1 _ = log1
                 LogMessage _ ts2 _ = log2
             in
               ts1 < ts2


build :: [LogMessage] -> MessageTree
build logs =
    foldl (\tree myLog -> insert myLog tree) Leaf logs


inOrder :: MessageTree -> [LogMessage]
inOrder tree =
    case tree of
      Leaf -> []
      Node left myLog right -> (inOrder left) ++ myLog : (inOrder right)


whatWentWrong :: [LogMessage] -> [String]
whatWentWrong =
    (map getInfo) . (filter isWrong) . inOrder . build
        where isWrong = \myLog ->
                        case myLog of
                          LogMessage (Error level) _ _ -> level > 50
                          _ -> False
              getInfo = \(LogMessage _ _ info) -> info


-- Test functions
testParsing :: FilePath -> IO [LogMessage]
testParsing file =
  parse <$> readFile file


testWhatWentWrong :: FilePath -> IO [String]
testWhatWentWrong file =
  whatWentWrong . parse <$> readFile file
