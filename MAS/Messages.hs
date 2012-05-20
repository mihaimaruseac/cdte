module MAS.Messages where

{-
Message types.
-}

import Control.Concurrent.Chan
import Control.Concurrent.MVar

data Message
  = None
  deriving (Show)

type Comm = (MVar Message, Chan Message)
