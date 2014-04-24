module Octopus.TQueue (
  dumpTQueue
) where

import Control.Concurrent.STM

dumpTQueue :: TQueue a -> STM [a]
dumpTQueue q =
    let collect acc = do
                      empty <- isEmptyTQueue q
                      case empty of
                        True -> return acc
                        False -> do
                          v <- readTQueue q
                          collect (v:acc)

        uncollect (x:xs) = do
                         unGetTQueue q x
                         uncollect xs
        uncollect [] = return ()

    in collect [] >>= \list -> uncollect list >> return list
