module MAS.AF where

{-
Represents the AF agent. Since this is the only one who knows the entire
configuration, this module is also responsible for getting the informations to
be displayed to output.
-}

import Data.List (groupBy)

import MAS.AP
import MAS.GenericTypes

import Debug.Trace

-- The AF agent state.
data AF = AF
  { numAgents :: Int
  , numSteps :: Int
  , leftOverPenalty :: Float
  , agentList :: [AP]
  , taskList :: [(Time, [Task])]
  } deriving (Show)

-- Parses system file and construct the agents then start the environment.
prepareAndLaunch :: String -> IO ()
prepareAndLaunch fname = do
  s <- readFile fname
  print $ parseFileContent s

-- Parses system file content. Build the AF agent.
parseFileContent :: String -> AF
parseFileContent s = parseFirstGroup ntlp a t
  where
    [ntlp, _, a, _, t] = if length g == 5 then g
      else error "Invalid system file (check to have two blank lines)"
    g = groupBy (\x y -> x /= "" && y /= "") $ lines s

-- Parses the first group of data in the system file.
parseFirstGroup :: [String] -> [String] -> [String] -> AF
parseFirstGroup [ntlp] a t = parseFirstLine ntlp a t
parseFirstGroup _ _ _ = error "Invalid system file (first line should be followed by blank)"

-- Parses the first line: number of agent, time steps and leftoverpenalty
parseFirstLine :: String -> [String] -> [String] -> AF
parseFirstLine s a ts
  | length fl == 3 = AF n t lp (parseAgents a n) (parseTasks ts)
  | otherwise = error "Invalid format for first line"
  where
    fl = words s
    [n, t] = map read $ take 2 fl
    lp = read $ fl !! 2

-- Parses the agent lists. (checks if the agent count is good).
parseAgents :: [String] -> Int -> [AP]
parseAgents l n
  | length l == n = doParseAgents $ zip l [1..]
  | otherwise = error "Invalid system file (declared agents and count of agents don't match)"

-- Parses the agent list.
doParseAgents :: [(String, ID)] -> [AP]
doParseAgents = map parseAgent

-- Parses a single agent.
parseAgent :: (String, ID) -> AP
parseAgent (s, i)
  | even (length ws) = error "Invalid system file (wrong agent specification)"
  | otherwise = AP i bdg caps
  where
    ws = words s
    bdg = read $ head ws
    caps = buildCapLists $ tail ws
    buildCapLists [] = []
    buildCapLists (cap:cost:ccs) = ((read cap), (read cost)) : buildCapLists ccs

-- Checks the length of the task lists and start parsing it.
parseTasks :: [String] -> [(Time, [Task])]
parseTasks l = doParseTasks (map (map read) (map words l)) 1 1

-- Parse tasks
doParseTasks :: [[ID]] -> Time -> ID -> [(Time, [Task])]
doParseTasks [] _ _ = []
doParseTasks (tt:ts) t i = (t, tl) : doParseTasks ts (t + 1) i'
  where
    (i', tl) = doParseTaskTime tt i
    doParseTaskTime [] i = (i, [])
    doParseTaskTime (t:ts) i = let (i', t') = doParseTaskTime ts (i + 1) in (i', (i, t) : t')
