module MAS.AP where

{-
Represents the AP agent. No AP agent has access to the system file, thus it is
constructed by giving the entire argument lists to the constructor.
-}

import MAS.GenericTypes

-- The AP agent state.
data AP = AP
  { id :: ID
  , budget :: Cost
  , caps :: [Cap]
  } deriving (Show)

