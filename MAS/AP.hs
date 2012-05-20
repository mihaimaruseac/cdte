{-# Language BangPatterns #-}

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
  incomingAP=inc, idAP=aid}) t r = do
    let all_tasks = lo ++ t ++ r
    -- get info about tasks
    taskInfo <- askAboutOthers afap a all_tasks
    writeChan afap $ DoneAsking aid
    print taskInfo
    -- try to solve some tasks
    -- TODO distribute tasks to other agents
    return ([], all_tasks)

askAboutOthers :: Chan Message -> AP -> [Task] -> IO [(Task, [AP])]
askAboutOthers afap a = mapM (askOne afap a)

askOne :: Chan Message -> AP -> Task -> IO (Task, [AP])
askOne afap a t@(tid, cid) = do
  putStrLn $ show a ++  " asking about " ++ show (tid, cid)
  writeChan afap $ AskCap a cid
  waitForReply [] (incomingAP a)
  where
    waitForReply ms c = do
      m <- readChan c
      case m of
        AnsCap cid' aps -> if cid' /= cid then waitForReply (m:ms) c else do
          putBackAll ms c
          putStrLn $ show a ++ " received " ++ show m
          return (t, aps)
        _ -> waitForReply (m:ms) c

buildAP :: ID -> Cost -> Cost -> [Cap] -> Chan Message -> Chan Message -> AP
buildAP i bdg lop caps afap incoming = AP i bdg caps afap incoming [] lop
