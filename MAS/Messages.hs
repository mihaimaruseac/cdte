module MAS.Messages where

{-
Message types. Add agent types here to prevent circular imports.
-}

import Control.Concurrent.Chan

import MAS.GenericTypes

type Comm = (Chan Message, Chan Message)

data Message
  = Tasks [Task] -- list of tasks to do this step (only af->ap)
  | WillDo AP [Task] [Task] -- list of tasks that AP will do and list of
                            -- leftovers (AP->AF)
  | AskCap AP ID -- ask which other AP can do tasks requiring ID (AP->AF)
  | AnsCap ID [AP] -- reply to the above request (AF->AP)
  | Cfp AP ID ID (Maybe Cost) -- cfp: me, task id, cap id, my cost (or Nothing) (AP->AP)
  | Deny AP ID Reason -- deny cfp: who denied, what task, what reason (AP->AP)
  | Accept AP ID Cost -- accept cfp: who accepts, what task, what cost (AP->AP)
  | Go ID ID -- AP can do task ID with cap ID (traded task) (AP->AP)
  | No ID ID -- AP cannot do task, given to other (AP->AP)
  | Notify Message -- notify AF about message between APs
  deriving (Show)

data Reason
  = NoProfit -- doing this task will not increase profit / social welfare
  | Unable -- I cannot do this task, why did you send it to me?
  | NoSpace -- I'd do this task but I don't have budget left
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
putBackAll [m] c = putStrLn ("putback " ++ show m) >> writeChan c m
putBackAll (m:ms) c = putBackAll ms c >> writeChan c m

pprintAP :: AP -> String
pprintAP a = "AP" ++ id ++ ": budget: " ++ bdg ++ " caps: " ++ capss
  where
    id = show $ idAP a
    bdg = show $ budget a
    capss = pprintCaps $ caps a
