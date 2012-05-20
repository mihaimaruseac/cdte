module MAS.AF where

{-
Represents the AF agent. Since this is the only one who knows the entire
configuration, this module is also responsible for getting the informations to
be displayed to output.
-}

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad (when, replicateM, forM_)
import Data.List (groupBy)

import MAS.AP
import MAS.GenericTypes
import MAS.Messages

import Debug.Trace

-- The AF agent state.
data AF = AF
  { numAgents :: Int
  , numSteps :: Int
  , leftOverPenalty :: Float
  , agentList :: [AP]
  , taskList :: [(Time, [Task])]
  , incoming :: Chan Message
  }

instance Show AF where
  show af = show (numAgents af) ++ ": " ++ concatMap show (agentList af)

-- Parses system file and construct the agents then start the environment.
prepareAndLaunch :: String -> IO ()
prepareAndLaunch fname = do
  af <- parseSystemFile fname
  displayStartInfo af
  launchSystem af

launchSystem :: AF -> IO ()
launchSystem af = do
  print "Forking threads"
  mapM_ (forkIO . agentLoopAP) $ agentList af
  terminate af
  print "Done"

-- wait for all termination messages
terminate :: AF -> IO ()
terminate af = doTerminate (numAgents af) (incoming af)
  where
    doTerminate 0 _ = return ()
    doTerminate n inc = do
      m <- readChan inc
      print m
      if isEnd m then doTerminate (n - 1) inc else doTerminate n inc

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
  return $ AF n tc lp (parseAgents a c n) (parseTasks t) inc

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
