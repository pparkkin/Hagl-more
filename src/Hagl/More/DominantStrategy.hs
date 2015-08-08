{-# LANGUAGE TypeFamilies #-}

module Hagl.More.DominantStrategy
    ( dominantStrategy
    , dominantStrategies
    )
where

import Hagl
import Data.List (delete)
import Control.Monad (mapM)
import Data.Maybe (fromMaybe)

class DominantStrategies g where
    type DStrats mv
    -- |Return dominant strategies for player
    dominantStrategies :: (Eq mv) => g mv -> PlayerID -> DStrats mv

instance DominantStrategies Normal where
    type DStrats mv = [mv]
    -- |Return a list of the dominant strategies for a player in a normal form game
    dominantStrategies g@(Normal np mvs os) p = filter (dominantStrategy g p) strategies
        where
          strategies = forPlayer p mvs

-- |Check whether or not the given strategy is a dominant strategy for the given player in a normal form game
dominantStrategy :: (Eq mv) => Normal mv -> PlayerID -> mv -> Bool
dominantStrategy g@(Normal np mvs os) p m = isMoveValid g p m && dominatesAll si sis
    where
      (order, si) = unzip (payoffsFor p m g)
      otherMoves = delete m (forPlayer p mvs)
      otherPayoffs = map (\om -> payoffsFor p om g) otherMoves
      sis = map (sortIntoOrder order) otherPayoffs

sortIntoOrder :: (Eq a) => [a] -> [(a, b)] -> [b]
sortIntoOrder ord os = fromMaybe [] $ mapM (`lookup` os) ord

payoffsFor :: (Eq mv) => PlayerID -> mv -> Normal mv -> [([mv], Float)]
payoffsFor pid m g = map resultPair ps
    where ps = filter (\ms -> forPlayer pid ms == m) (profiles g)
          resultPair p = (others p,  own (getPayoff g p))
          others (ByPlayer ms) = take (pid-1) ms ++ drop pid ms
          own = forPlayer pid

dominatesAll :: [Float] -> [[Float]] -> Bool
dominatesAll d = foldl (\acc o -> acc && dominates d o) True

dominates :: [Float] -> [Float] -> Bool
dominates as bs = foldl (\acc (a, b) -> acc && a >= b) True $ zip as bs

