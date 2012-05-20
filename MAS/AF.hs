module MAS.AF where

{-
Represents the AF agent. Since this is the only one who knows the entire
configuration, this module is also responsible for getting the informations to
be displayed to output.
-}

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad (when, replicateM, forM_, unless)
import Data.List (groupBy)

import MAS.AP
import MAS.GenericTypes
import MAS.Messages

import Debug.Trace

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

instance Show AF where
  show af = show (numAgents af) ++ ": " ++ concatMap show (agentList af)

-- Parses system file and construct the agents then start the environment.
prepareAndLaunch :: String -> IO ()
prepareAndLaunch fname = do
  af <- parseSystemFile fname
  displayStartInfo af
  launchSystem af
  agentLoopAF af 0

agentLoopAF :: AF -> Time -> IO ()
agentLoopAF af t = do
  putStrLn ""
  putStrLn $ "Cycle " ++ show t ++ ", phase 1:"
  doLoop af t

doLoop :: AF -> Time -> IO ()
doLoop af@(AF { taskList = tl }) t'
  | tl == [] = agentLoopNoTasksToSend af t'
  | otherwise = agentLoopSendingTasks af t'

agentLoopPhase2 :: AF -> Time -> IO ()
agentLoopPhase2 af@(AF { agentList=ags, taskList=tl, profit=p }) t = do
  allTasks <- receiveTasksDone af
  putStrLn ""
  putStrLn $ "Cycle " ++ show t ++ ", phase 2:"
  printAllTasks allTasks
  let pNow = 42 -- TODO
  let p' = p + pNow -- TODO
  putStrLn $ "Total profit: " ++ show p' ++ " (now: " ++ show pNow ++ ")"
  let af' = af { profit = p' }
  unless (tl == [] && all finished ags) $ agentLoopAF af' (t + 1)

printAllTasks :: [(ID, [Task], [Task])] -> IO ()
printAllTasks = mapM_ printTasksOneAgent

printTasksOneAgent :: (ID, [Task], [Task]) -> IO ()
printTasksOneAgent (aid, todo, leftOver) = do
  when (todo /= []) $ putStrLn $ "AP" ++ show aid ++ " executes: " ++ pprintTasks todo
  when (leftOver /= []) $ putStrLn $ "AP" ++ show aid ++ " postpones " ++ pprintTasks leftOver

agentLoopSendingTasks :: AF -> Time -> IO ()
agentLoopSendingTasks af@(AF { taskList = (t, _):_ }) t'
  | t == t' + 1 = doSendTasks af t'
  | otherwise = agentLoopNoTasksToSend af t' -- only wait

agentLoopNoTasksToSend :: AF -> Time -> IO ()
agentLoopNoTasksToSend af t = do
  -- TODO: wait for negotiations
  agentLoopPhase2 af t

doSendTasks :: AF -> Time -> IO ()
doSendTasks af@(AF { agentList=ag, taskList=(_, ts):tss }) t = do
  let otd = computeOptimumTaskDistribution af ts
  distributeTasks $ fillIn otd ag
  let af' = af { taskList = tss }
  agentLoopNoTasksToSend af' t
  where
    fillIn l [] = l
    fillIn l (x:xs)
      | x `elem` map fst l = fillIn l xs
      | otherwise = (x, []) : fillIn l xs

receiveTasksDone :: AF -> IO [(ID, [Task], [Task])]
receiveTasksDone a@(AF {numAgents=n, incoming=c}) = doRTD n c []
  where
    doRTD 0 _ _ = return []
    doRTD n inc ms = do
      m <- readChan c
      case m of
        WillDo aid todo lo -> do
          putBackAll ms inc
          ret <- doRTD (n - 1) inc ms
          return $ (aid, todo, lo) : ret
        _ -> doRTD n inc (m:ms)

computeOptimumTaskDistribution :: AF -> [Task] -> [(AP, [Task])]
computeOptimumTaskDistribution af t = [(head $ agentList af, t)]

distributeTasks :: [(AP, [Task])] -> IO ()
distributeTasks = mapM_ distributeTasksToOneAgent

distributeTasksToOneAgent :: (AP, [Task]) -> IO ()
distributeTasksToOneAgent (a@AP { idAP = aid, incomingAP = c }, t) = do
  when (t /= []) $ putStrLn $ "AF -> AP" ++ show aid ++ ": " ++ pprintTasks t
  writeChan c $ Tasks t

launchSystem :: AF -> IO ()
launchSystem af = mapM_ (forkIO . agentLoopAP) $ agentList af

-- Parse system file
parseSystemFile :: String -> IO AF
parseSystemFile fname = do
  s <- readFile fname
  let g = groupBy (\x y -> x /= "" && y /= "") $ lines s
  when (length g /= 5) $ error "Invalid system file (no two blank lines)"
  let [ntlp, _, a, _, t] = g
  when (length ntlp /= 1) $ error "Invalid system file (first line should be followed by blank)"
  let fl = words . head $ ntlp
  when (length fl /= 3) $ error "Invalid format for first line"
  let [n, tc] = map read $ take 2 fl
  let lp = read $ fl !! 2
  -- build communication primitives
  inc <- newChan
  chans <- replicateM n newChan
  let c = map (\x-> (inc, x)) chans
  -- return agent
  return $ AF n tc lp (parseAgents a c n) (parseTasks t) inc 0

-- Prints the startup information
displayStartInfo :: AF -> IO ()
displayStartInfo af = do
  putStrLn $ "System start: " ++ show (numAgents af) ++ " agents."
  mapM_ print $ agentList af

-- Parses the agent lists. (checks if the agent count is good).
parseAgents :: [String] -> [Comm] -> Int -> [AP]
parseAgents l c n
  | length l == n = doParseAgents $ zip (zip l [1..]) c
  | otherwise = error "Invalid system file (declared agents and count of agents don't match)"

-- Parses the agent list.
doParseAgents :: [((String, ID), Comm)] -> [AP]
doParseAgents = map parseAgent

-- Parses a single agent.
parseAgent :: ((String, ID), Comm) -> AP
parseAgent ((s, i), (afap, incoming))
  | even (length ws) = error "Invalid system file (wrong agent specification)"
  | otherwise = buildAP i bdg caps afap incoming
  where
    ws = words s
    bdg = read $ head ws
    caps = buildCapLists $ tail ws
    buildCapLists [] = []
    buildCapLists (cap:cost:ccs) = (read cap, read cost) : buildCapLists ccs

-- Checks the length of the task lists and start parsing it.
parseTasks :: [String] -> [(Time, [Task])]
parseTasks l = doParseTasks (map (map read . words) l) 1 1

-- Parse tasks
doParseTasks :: [[ID]] -> Time -> ID -> [(Time, [Task])]
doParseTasks [] _ _ = []
doParseTasks (tt:ts) t i = (t, tl) : doParseTasks ts (t + 1) i'
  where
    (i', tl) = doParseTaskTime tt i
    doParseTaskTime [] i = (i, [])
    doParseTaskTime (t:ts) i = let (i', t') = doParseTaskTime ts (i + 1) in (i', (i, t) : t')
