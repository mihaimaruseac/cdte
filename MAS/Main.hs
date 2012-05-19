module MAS.Main where

import System.Environment (getArgs)

import MAS.AF

-- Main entry point of the application.
mainMAS :: IO ()
mainMAS = do
  args <- getArgs
  prepareAndLaunch $ getSystemFile args

-- Returns the name of the file containing the description of the system.
getSystemFile :: [String] -> String
getSystemFile [] = "system.txt"
getSystemFile [f] = f
getSystemFile _ = error "Provide only the filename."
