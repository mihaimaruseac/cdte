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
  }

agentLoopAP :: AP -> IO ()
agentLoopAP ap = do
  print ap
  print "Started"
  writeChan (afap ap) $ None 1
  writeChan (afap ap) $ None 2
  writeChan (afap ap) $ None 3
  writeChan (afap ap) $ None 4
  writeChan (afap ap) $ None 5
  writeChan (afap ap) $ None 6
  writeChan (afap ap) $ None 7
  writeChan (afap ap) $ None 8
  writeChan (afap ap) $ None 9
  writeChan (afap ap) $ None 10
  writeChan (afap ap) $ None 11
  writeChan (afap ap) $ None 12
  writeChan (afap ap) $ End $ idAP ap

pprintAP :: AP -> String
pprintAP a = "AP" ++ id ++ ": budget: " ++ bdg ++ " caps: " ++ capss
  where
    id = show $ idAP a
    bdg = show $ budget a
    capss = pprintCaps $ caps a

instance Show AP where
  show = pprintAP
