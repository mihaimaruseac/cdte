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
    let proposedTasksByMe = map fst toProposeTasks
    forM_ toProposeTasks (proposeTasksToAgents a)
    writeChan afap $ DoneCfp aid
    proposedTaskList <- waitForAllCfpDone a
    let helpingTasks = filter (\(_,m,_) -> m == Nothing) proposedTaskList
    let maybeTasks = proposedTaskList \\ helpingTasks
    -- try to do as many of my tasks as possible
    let (newBudget, todoTasks, toLeaveTasks) = planMine myTasks b c
    -- then, try to do as many of the helping tasks as possible
    let (newBudget', todoTasks', toLeaveTasks') = planHelping (sortBy (incomingSort c) helpingTasks) newBudget c
    -- plan maybes, as many as possible, after filtering non-profitable ones
    let nonProfitables = filter (nonProfitable c) maybeTasks
    let maybeTasks' = maybeTasks \\ nonProfitables
    let (newBudget'', todoTasks'', toLeaveTasks'') = planHelping (sortBy (incomingSort c) maybeTasks') newBudget' c
    -- deny tasks
    doSendDenyOutOfBudget a afap $ toLeaveTasks' ++ toLeaveTasks''
    doSendDenyNonProfitable a afap nonProfitables
    -- accept tasks
    doAccept a afap $ todoTasks' ++ todoTasks''
    writeChan afap $ DoneReply aid
    -- wait for accept/deny and decide what to do
    (denied, accepted) <- waitForAllReplyDone a proposedTasksByMe
    print ("MM-da", denied, accepted, aid)
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

nonProfitable :: [Cap] -> IncomingTask -> Bool
nonProfitable cap ((_, c), Just x, _) = case lookup c cap of
  Nothing -> error "Again, impossible."
  Just c1 -> c1 > x
nonProfitable _ _ = error "Again, impossible."

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

doSendDenyOutOfBudget :: AP -> Chan Message -> [IncomingTask] -> IO ()
doSendDenyOutOfBudget a afap = mapM_ (sendDeny NoSpace a afap)

sendDeny :: Reason -> AP -> Chan Message -> IncomingTask -> IO ()
sendDeny r a@(AP {idAP=sid}) afap ((tid, _), _, AP {idAP=rid, incomingAP=apap}) = do
  let m = Deny a tid r
  writeChan apap m
  writeChan afap $ Notify sid rid m

doSendDenyNonProfitable :: AP -> Chan Message -> [IncomingTask] -> IO ()
doSendDenyNonProfitable a afap = mapM_ (sendDeny NoProfit a afap)

doAccept :: AP -> Chan Message -> [IncomingTask] -> IO ()
doAccept a afap = mapM_ (sendAccept a afap)

sendAccept :: AP -> Chan Message -> IncomingTask -> IO ()
sendAccept a@(AP {idAP=sid, caps=c}) afap ((tid, cid), _, AP {idAP=rid, incomingAP=apap}) = do
  let m = Accept a tid cost
  writeChan apap m
  writeChan afap $ Notify sid rid m
  where
    cost = case lookup cid c of
      Nothing -> error "Are you sure?"
      Just x -> x

waitForAllCfpDone :: AP -> IO [(Task, Maybe Cost, AP)]
waitForAllCfpDone a@(AP {incomingAP=c}) = recvAllCfpDone [] []
  where
    recvAllCfpDone ms r = do
      m <- readChan c
      case m of
        AllCfpDone -> putBackAll ms c >> return r
        Cfp ag tid cid agCost -> recvAllCfpDone ms $ ((tid, cid), agCost, ag) : r
        _ -> recvAllCfpDone (m:ms) r

waitForAllReplyDone :: AP -> [Task] -> IO ([Task], [IncomingTask])
waitForAllReplyDone a@(AP {incomingAP=c}) ts = recvAllReplyDone [] ([], [])
  where
    recvAllReplyDone ms r@(rd, ra) = do
      m <- readChan c
      case m of
        AllReplyDone -> putBackAll ms c >> return r
        Accept a id cost -> recvAllReplyDone ms (rd, (findTask id, Just cost, a) : ra)
        Deny a id cost -> recvAllReplyDone ms (findTask id : rd, ra)
        _ -> print ("msg", m) >> recvAllReplyDone (m:ms) r
    findTask tid = doFindTask tid ts
    doFindTask tid [] = error "Dreaming..."
    doFindTask tid (t:ts)
      | tid == fst t = t
      | otherwise = doFindTask tid ts

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
