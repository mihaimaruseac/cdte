module MAS.AP where

{-
Represents the AP agent. No AP agent has access to the system file, thus it is
constructed by giving the entire argument lists to the constructor.
-}

import Control.Concurrent.Chan
import Control.Concurrent.MVar

import MAS.GenericTypes
import MAS.Messages

-- The AP agent state.
data AP = AP
  { idAP :: ID
  , budget :: Cost
  , caps :: [Cap]
  , afap :: Chan Message -- to AF (incoming for AF)
  , incomingAP :: Chan Message -- incoming to me
  , tasksToDo :: [Task] -- received now from AF
  , leftOvers :: [Task] -- task not done previously
  , tasksFromOthers :: [Task] -- from other agents
  }

-- received from AF, received from other APs and leftovers
tasksToConsider :: AP -> [Task]
tasksToConsider a@(AP { tasksToDo=tdd, leftOvers=lo, tasksFromOthers=tfo})
  = tdd ++ lo ++ tfo

-- Return True if I have no task to do (at all).
finished :: AP -> Bool
finished a@(AP { leftOvers=lo }) = lo == []

agentLoopAP :: AP -> IO ()
agentLoopAP ap = do
  t <- receiveTasks ap []
  print $ "AP:" ++ show (ap, t)

receiveTasks :: AP -> [Message] -> IO [Task]
receiveTasks a@(AP { incomingAP=inc }) ms = do
  m <- readChan inc
  case m of
    Tasks t -> do
      putBackAll ms inc
      return t
    _ -> receiveTasks a (m:ms)

buildAP :: ID -> Cost -> [Cap] -> Chan Message -> Chan Message -> AP
buildAP i bdg caps afap incoming = AP i bdg caps afap incoming [] [] []

pprintAP :: AP -> String
pprintAP a = "AP" ++ id ++ ": budget: " ++ bdg ++ " caps: " ++ capss
  where
    id = show $ idAP a
    bdg = show $ budget a
    capss = pprintCaps $ caps a

instance Show AP where
  show = pprintAP
