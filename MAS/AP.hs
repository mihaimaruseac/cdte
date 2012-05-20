module MAS.AP where

{-
Represents the AP agent. No AP agent has access to the system file, thus it is
constructed by giving the entire argument lists to the constructor.
-}

import Control.Concurrent.Chan
import Control.Concurrent.MVar

import MAS.GenericTypes
import MAS.Messages

-- Return True if I have no task to do (at all).
finished :: AP -> Bool
finished a@(AP { leftOvers=lo }) = lo == []

agentLoopAP :: AP -> IO ()
agentLoopAP a@(AP {idAP=aid, afap=afap}) = do
  t <- receiveTasks a []
  (td, lo) <- planifyTasks a t []
  let a' = a {leftOvers = lo}
  writeChan afap $ WillDo a' td lo
  agentLoopAP a' -- loop until killed by AF

receiveTasks :: AP -> [Message] -> IO [Task]
receiveTasks a@(AP { incomingAP=inc }) ms = do
  m <- readChan inc
  case m of
    Tasks t -> do
      putBackAll ms inc
      return t
    _ -> receiveTasks a (m:ms)

planifyTasks :: AP -> [Task] -> [Task] -> IO ([Task], [Task])
planifyTasks a@(AP {budget=b, caps=c, leftOvers=lo, afap=afap,
  incomingAP=inc}) t r = do
    let all_tasks = lo ++ t ++ r
    -- TODO distribute tasks to other agents
    return ([], all_tasks)

buildAP :: ID -> Cost -> [Cap] -> Chan Message -> Chan Message -> AP
buildAP i bdg caps afap incoming = AP i bdg caps afap incoming []
