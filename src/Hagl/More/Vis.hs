{-# LANGUAGE ScopedTypeVariables #-}

module Hagl.More.Vis where

import Data.Maybe (fromMaybe)
import Data.List (nub
                , intercalate)

import Hagl

import Data.GraphViz
import Data.GraphViz.Attributes
import Data.GraphViz.Commands

-- For example
-- TODO: Remove!
import Hagl.Examples.Crisis

extensiveNodes :: (Eq mv) => Extensive mv -> [Extensive mv]
extensiveNodes = nub . bfs

zipWithIndex :: [a] -> [(a, Int)]
zipWithIndex = (`zip` [0..])

edgesFrom :: forall mv . (Eq mv)
          => [(Extensive mv, Int)]
          -> (Extensive mv, Int)
          -> [((Extensive mv, Int), mv, (Extensive mv, Int))]
edgesFrom _ ((Discrete _ []), _) = []
edgesFrom nis n@((Discrete _ es), _) = map edgepair es
    where
        edgepair :: (mv, Extensive mv) -> ((Extensive mv, Int), mv, (Extensive mv, Int))
        edgepair (a, n') = (n, a, (n', fromMaybe (-1) (lookup n' nis)))

nodeId :: (Extensive mv, Int) -> String
nodeId = show . snd

actionLabel :: (Show mv) => Node () mv -> String
actionLabel n = case nodeAction n of
    Decision i -> show i
    Chance d -> "Chance " ++ intercalate "\n" (map show d)
    Payoff (ByPlayer ps) -> show ps

nodeLabel :: (Show mv) => (Extensive mv, Int) -> String
nodeLabel ((Discrete n _), i)= actionLabel n

makeNode :: (Show mv) => (Extensive mv, Int) -> DotNode String
makeNode ni = DotNode (nodeId ni) [toLabel (nodeLabel ni)]

edgeId :: (Extensive mv, Int) -> (Extensive mv, Int) -> String
edgeId n1 n2 = (nodeId n1) ++ " -> " ++ (nodeId n2)

edgeLabel :: (Eq mv, Show mv) => ((Extensive mv, Int), mv, (Extensive mv, Int)) -> String
edgeLabel (_, a, _) = show a

makeEdge :: (Eq mv, Show mv) => ((Extensive mv, Int), mv, (Extensive mv, Int)) -> DotEdge String
makeEdge e@(n1, a, n2) = DotEdge (nodeId n1) (nodeId n2) [toLabel (edgeLabel e)]

makeEdges :: (Eq mv, Show mv) => [(Extensive mv, Int)] -> (Extensive mv, Int) -> [DotEdge String]
makeEdges nis ni = map makeEdge (edgesFrom nis ni)

extensiveToDot :: (Eq mv, Show mv) => Extensive mv -> DotGraph String
extensiveToDot g = DotGraph { strictGraph = False
                            , directedGraph = True
                            , graphID = Nothing
                            , graphStatements = DotStmts { attrStmts = []
                                                         , subGraphs = []
                                                         , nodeStmts = nodes
                                                         , edgeStmts = edges }}
    where
        ns = zipWithIndex $ extensiveNodes g
        nodes = map makeNode ns
        edges = concatMap (makeEdges ns) ns

-- Example
-- TODO: Remove!
generateTestPng :: IO String
generateTestPng = runGraphviz (extensiveToDot crisis) Png "test.png"

