module MAS.Messages where

{-
Message types. Add agent types here to prevent circular imports.
-}

import Control.Concurrent.Chan

import MAS.GenericTypes

type Comm = (Chan Message, Chan Message)
data Message
  = None Int
  | Tasks [Task] -- list of tasks to do this step (only af->ap)
  | WillDo AP [Task] [Task] -- list of tasks that AP will do and list of
                            -- leftovers (AP->AF)
  | End ID
  deriving (Show)

-- The AP agent state.
data AP = AP
  { idAP :: ID
  , budget :: Cost
  , caps :: [Cap]
  , afap :: Chan Message -- to AF (incoming for AF)
  , incomingAP :: Chan Message -- incoming to me
  , leftOvers :: [Task] -- task not done previously
  }

-- The AF agent state.
data AF = AF
  { numAgents :: Int
  , numTasks :: Int
  , leftOverPenalty :: Cost
  , agentList :: [AP]
  , taskList :: [(Time, [Task])]
  , incoming :: Chan Message
  , profit :: Cost
  }

instance Show AP where
  show = pprintAP

instance Eq AP where
  AP {idAP=id1} == AP {idAP=id2} = id1 == id2

instance Show AF where
  show af = show (numAgents af) ++ ": " ++ concatMap show (agentList af)

putBackAll :: [Message] -> Chan Message -> IO ()
putBackAll [] _ = return ()
putBackAll [m] c = print ("putback" ++ show m) >> writeChan c m
putBackAll (m:ms) c = putBackAll ms c >> writeChan c m

pprintAP :: AP -> String
pprintAP a = "AP" ++ id ++ ": budget: " ++ bdg ++ " caps: " ++ capss
  where
    id = show $ idAP a
    bdg = show $ budget a
    capss = pprintCaps $ caps a
