{-# Language BangPatterns #-}

module MAS.AP where

{-
Represents the AP agent. No AP agent has access to the system file, thus it is
constructed by giving the entire argument lists to the constructor.
-}

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad (when, replicateM, forM_, unless)
import Data.List (sortBy, (\\))

import MAS.GenericTypes
import MAS.Messages

type IncomingTask = (Task, Maybe Cost, AP)

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
  incomingAP=inc, idAP=aid, lop=lop}) t r = do
    let all_tasks = lo ++ t ++ r
    -- get info about tasks
    taskInfo <- askAboutOthers afap a all_tasks
    writeChan afap $ DoneAsking aid
    -- find tasks only I can do
    let myTasks = sortBy (myCapacity c) $ map fst $ filter (\(_,l) -> l == []) taskInfo
    -- send proposals
    let toProposeTasks = filter (\(_, l) -> l /= []) taskInfo
    forM_ toProposeTasks (proposeTasksToAgents a)
    writeChan afap $ DoneCfp aid
    proposedTaskList <- waitForAllCfpDone a
    let helpingTasks = filter (\(_,m,_) -> m == Nothing) proposedTaskList
    let maybeTasks = proposedTaskList \\ helpingTasks
    -- try to do as many of my tasks as possible
    let (newBudget, todoTasks, toLeaveTasks) = planMine myTasks b c
    print ("MM-mine", newBudget, todoTasks, toLeaveTasks, aid)
    -- then, try to do as many of the helping tasks as possible
    let (newBudget', todoTasks', toLeaveTasks') = planHelping (sortBy (incomingSort c) helpingTasks) newBudget c
    print ("MM-help", newBudget', todoTasks', toLeaveTasks', aid)
    -- TODO: discard wrong tasks
    -- try to solve some tasks
    -- TODO distribute tasks to other agents
    return ([], all_tasks)

myCapacity :: [Cap] -> Task -> Task -> Ordering
myCapacity cap (_, c1) (_, c2) =
  case lookup c1 cap of
    Nothing -> error "Invalid task list (no one can do task)"
    Just co1 -> case lookup c2 cap of
      Nothing -> error "Invalid task list (no one can do task)"
      Just co2 -> co1 `compare` co2

incomingSort :: [Cap] -> IncomingTask -> IncomingTask -> Ordering
incomingSort cap ((_, c1), _, _) ((_, c2), _, _) =
  case lookup c1 cap of
    Nothing -> error "This shouldn't happen"
    Just co1 -> case lookup c2 cap of
      Nothing -> error "This shouldn't happen"
      Just co2 -> co1 `compare` co2

planMine :: [Task] -> Cost -> [Cap] -> (Cost, [Task], [Task])
planMine ts b c = doPlan ts b c [] []
  where
    doPlan [] b _ todo toleave = (b, todo, toleave)
    doPlan all@(t@(tid, cid):ts) b c todo toleave = case lookup cid c of
      Nothing -> error "This should not have happened"
      Just cost -> if cost <= b then doPlan ts (b - cost) c (t:todo) toleave
        else doPlan [] b c todo all

planHelping :: [IncomingTask] -> Cost -> [Cap] -> (Cost, [IncomingTask], [IncomingTask])
planHelping ts b c = doPlan ts b c [] []
  where
    doPlan [] b _ todo toleave = (b, todo, toleave)
    doPlan all@(t@((tid, cid), _, _):ts) b c todo toleave = case lookup cid c of
      Nothing -> error "Impossible situation"
      Just cost -> if cost <= b then doPlan ts (b - cost) c (t:todo) toleave
        else doPlan [] b c todo all

waitForAllCfpDone :: AP -> IO [(Task, Maybe Cost, AP)]
waitForAllCfpDone a@(AP {incomingAP=c}) = recvAllCfpDone [] []
  where
    recvAllCfpDone ms r = do
      m <- readChan c
      case m of
        AllCfpDone -> putBackAll ms c >> return r
        Cfp ag tid cid agCost -> recvAllCfpDone ms $ ((tid, cid), agCost, ag) : r
        _ -> recvAllCfpDone (m:ms) r

askAboutOthers :: Chan Message -> AP -> [Task] -> IO [(Task, [AP])]
askAboutOthers afap a = mapM (askOne afap a)

askOne :: Chan Message -> AP -> Task -> IO (Task, [AP])
askOne afap a t@(tid, cid) = do
  writeChan afap $ AskCap a cid
  waitForReply [] (incomingAP a)
  where
    waitForReply ms c = do
      m <- readChan c
      case m of
        AnsCap cid' aps -> if cid' /= cid then waitForReply (m:ms) c else do
          putBackAll ms c
          return (t, aps)
        _ -> waitForReply (m:ms) c

proposeTasksToAgents :: AP -> (Task, [AP]) -> IO ()
proposeTasksToAgents a (task, alist) = mapM_ (proposeTaskToAgent a task) alist

proposeTaskToAgent :: AP -> Task -> AP -> IO ()
proposeTaskToAgent a@(AP {caps=caps, idAP=sid, afap=afap}) (tid, cid)
  (AP {incomingAP=apap, idAP=rid}) = do
    let msg = Cfp a tid cid (lookup cid caps)
    writeChan apap msg
    writeChan afap $ Notify sid rid msg

buildAP :: ID -> Cost -> Cost -> [Cap] -> Chan Message -> Chan Message -> AP
buildAP i bdg lop caps afap incoming = AP i bdg caps afap incoming [] lop
