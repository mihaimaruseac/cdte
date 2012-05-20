module MAS.Messages where

{-
Message types.
-}

import Control.Concurrent.Chan

import MAS.GenericTypes

type Comm = (Chan Message, Chan Message)
data Message
  = None Int
  | Tasks [Task] -- list of tasks to do this step (only af->ap)
  | End ID
  deriving (Show)

isEnd :: Message -> Bool
isEnd (End _) = True
isEnd _ = False
