{-# LANGUAGE TypeFamilies #-}

module Hagl.More.PureStrategies
    ( pureStrategies
    )
where

import Hagl
import Data.List (delete)
import Control.Monad (mapM)
import Data.Maybe (fromMaybe)

mapExtensive :: ((Node d mv, [mv]) -> a) -> Discrete d mv -> [a]
mapExtensive f (Discrete n es) = f (n, mvs) : concatMap (mapExtensive f . snd) es
    where
        mvs = map fst es

class PureStrategies g where
    type Strat mv
    -- |Return the list of pure strategies for a player
    pureStrategies :: g mv -> PlayerID -> Strat mv

instance PureStrategies (Discrete d) where
    type Strat mv = [[mv]]
    -- |Return the list of pure strategied for a player in a extended form game
    pureStrategies g p = sequence $ filter (not . null) $ mapExtensive (movesForPlayer p) g
        where
            movesForPlayer :: PlayerID -> (Node d mv, [mv]) -> [mv]
            movesForPlayer p (n, mvs) = case n of
                (_, Decision p') | p == p' -> mvs
                _ -> []

instance PureStrategies Normal where
    type Strat mv = [[mv]]
    -- |Return the list of pure strategies for a player in a normal form game
    pureStrategies (Normal _ mvs _) p = map (:[]) $ forPlayer p mvs



