module MAS (mainMAS) where

{-
This modules is used to reexport the `mainMAS` function such that `Main.hs`
can see it and declare it to be the main entry point of the `cdte`
application.
-}

import MAS.Main (mainMAS)
