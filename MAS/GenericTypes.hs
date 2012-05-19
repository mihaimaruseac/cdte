module MAS.GenericTypes where

{-
Common types involved both in AP and AF agents specifications.
-}

type Cap = (ID, Cost)
type ID = Int
type Cost = Float
type Task = (ID, ID)
type Time = Int

pprintCaps :: [Cap] -> String
pprintCaps = init . concatMap pprintCap

pprintCap :: Cap -> String
pprintCap (id, c) = show id ++ " (cost " ++ show c ++ ") "

