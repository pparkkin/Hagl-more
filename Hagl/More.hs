
module Hagl.More
    ( dominant
    , dominantStrategies )
where

import Hagl
import Data.List (delete)
import Control.Monad (mapM)
import Data.Maybe (fromMaybe)


dominantStrategies :: (Eq mv) => Normal mv -> PlayerID -> [mv]
dominantStrategies g@(Normal np mvs os) p = filter (dominant g p) strategies
    where
      strategies = forPlayer p mvs

dominant :: (Eq mv) => Normal mv -> PlayerID -> mv -> Bool
dominant g@(Normal np mvs os) p m = if not $ isMoveValid g p m
                                    then False
                                    else dominatesAll si sis
    where
      (order, si) = unzip (payoffsFor p m g)
      otherMoves = delete m (forPlayer p mvs)
      otherPayoffs = (map (\om -> payoffsFor p om g) otherMoves)
      sis = map (sortIntoOrder order) otherPayoffs

sortIntoOrder :: (Eq a) => [a] -> [(a, b)] -> [b]
sortIntoOrder ord os = fromMaybe [] $ mapM (flip lookup os) ord

payoffsFor :: (Eq mv) => PlayerID -> mv -> Normal mv -> [([mv], Float)]
payoffsFor pid m g = map resultPair ps
    where ps = filter (\ms -> forPlayer pid ms == m) (profiles g)
          resultPair p = (others p,  own (getPayoff g p))
          others (ByPlayer ms) = take (pid-1) ms ++ drop pid ms
          own = forPlayer pid

dominatesAll :: [Float] -> [[Float]] -> Bool
dominatesAll d os = foldl (\acc o -> acc && dominates d o) True os

dominates :: [Float] -> [Float] -> Bool
dominates as bs = foldl (\acc (a, b) -> acc && a >= b) True $ zip as bs

