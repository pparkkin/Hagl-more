
module Hagl.More
    ( dominant
    , dominantStrategies
    , pureStrategies )
where

import Hagl
import Data.List (delete)
import Control.Monad (mapM)
import Data.Maybe (fromMaybe)

class PureStrategies g where
    pureStrategies :: g mv -> PlayerID -> [[mv]]

--instance PureStrategies Discrete where
--    pureStrategies (Discrete n es) p = case n of
--                                         (_, Decision p') -> collectEdges es (p' == p)
--                                         _ -> []
--        where
--          collectEdges :: [Edge s mv] -> Bool -> [[mv]]
--          collectEdges es True = concatMap (\(mv, t) ->
--                                                map (mv :) (pureStrategies t p)) es
--          collectEdges es False = concatMap (\(_, t) -> pureStrategies t p) es

instance PureStrategies Normal where
    pureStrategies (Normal _ mvs _) p = map (:[]) $ forPlayer p mvs

-- |Return a list of the dominant strategies for a player in a normal form game
dominantStrategies :: (Eq mv) => Normal mv -> PlayerID -> [mv]
dominantStrategies g@(Normal np mvs os) p = filter (dominantStrategy g p) strategies
    where
      strategies = forPlayer p mvs

-- |Check whether or not the given strategy is a dominant strategy for the given player in a normal form game
dominantStrategy :: (Eq mv) => Normal mv -> PlayerID -> mv -> Bool
dominantStrategy g@(Normal np mvs os) p m = if not $ isMoveValid g p m
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

