module MAS.AF where

import Data.List (groupBy)

import Debug.Trace

{-
Represents the AF agent. Since this is the only one who knows the entire
configuration, this module is also responsible for getting the informations to
be displayed to output.
-}

-- The AF agent state.
data AF = AF
  { numAgents :: Int
  , numSteps :: Int
  , leftOverPenalty :: Float
  , agentList :: [String]
  , taskList :: [String]
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
  | length fl == 3 = AF n t lp a ts
  | otherwise = error "Invalid format for first line"
  where
    fl = words s
    [n, t] = map read $ take 2 fl
    lp = read $ fl !! 2
