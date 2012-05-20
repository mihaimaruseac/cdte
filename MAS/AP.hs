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
  , leftOvers :: [Task] -- task not done previously
  }

-- Return True if I have no task to do (at all).
finished :: AP -> Bool
finished a@(AP { leftOvers=lo }) = lo == []

agentLoopAP :: AP -> IO ()
agentLoopAP a@(AP { afap=afap }) = do
  t <- receiveTasks a []
  (td, lo) <- planifyTasks a t
  print $ "X" ++ show (td, lo)
  print "ap"
  print a
  writeChan afap $End 42
  print "Done"

receiveTasks :: AP -> [Message] -> IO [Task]
receiveTasks a@(AP { incomingAP=inc }) ms = do
  m <- readChan inc
  case m of
    Tasks t -> do
      putBackAll ms inc
      return t
    _ -> receiveTasks a (m:ms)

planifyTasks :: AP -> [Task] -> IO ([Task], [Task])
planifyTasks a@(AP {budget=b, caps=c, leftOvers=lo, afap=afap, incomingAP=inc}) t = do
  print (t, lo)
  return (t, lo)

buildAP :: ID -> Cost -> [Cap] -> Chan Message -> Chan Message -> AP
buildAP i bdg caps afap incoming = AP i bdg caps afap incoming []

pprintAP :: AP -> String
pprintAP a = "AP" ++ id ++ ": budget: " ++ bdg ++ " caps: " ++ capss
  where
    id = show $ idAP a
    bdg = show $ budget a
    capss = pprintCaps $ caps a

instance Show AP where
  show = pprintAP
