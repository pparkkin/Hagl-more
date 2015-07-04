{-# LANGUAGE ScopedTypeVariables #-}

module Hagl.More.Vis where

import Data.Maybe (fromMaybe)
import Data.List (nub)

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

extensiveEdge :: Extensive mv -> ExtEdge mv -> (Extensive mv, Extensive mv)
extensiveEdge n (_, n') = (n, n')

extensiveEdges :: Extensive mv -> [(Extensive mv, Extensive mv)]
extensiveEdges (Discrete _ []) = []
extensiveEdges n@(Discrete a es) = map (n `extensiveEdge`) es

nodesFrom :: forall mv . (Eq mv) => [(Extensive mv, Int)] -> (Extensive mv, Int) -> [(Extensive mv, Int)]
nodesFrom _ ((Discrete _ []), _) = []
nodesFrom nis ((Discrete _ es), _) = map (index . snd) es
    where
        index :: Extensive mv -> (Extensive mv, Int)
        index n = (n, fromMaybe (-1) (lookup n nis))

nodeId :: (Extensive mv, Int) -> String
nodeId = show . snd

-- TODO
actionLabel :: (Show mv) => Node () mv -> String
actionLabel n = case nodeAction n of
    Decision i -> show i
    Chance d -> "Chance " ++ show d
    Payoff (ByPlayer ps) -> "Payoff " ++ show ps

nodeLabel :: (Show mv) => (Extensive mv, Int) -> String
nodeLabel ((Discrete n _), i)= show i ++ ": " ++ actionLabel n

makeNode :: (Show mv) => (Extensive mv, Int) -> DotNode String
makeNode ni = DotNode (nodeId ni) [toLabel (nodeLabel ni)]

edgeId :: (Extensive mv, Int) -> (Extensive mv, Int) -> String
edgeId n1 n2 = (nodeId n1) ++ " -> " ++ (nodeId n2)

-- TODO: What if multiple moves go to the same target node?
edgeLabel :: (Eq mv, Show mv) => (Extensive mv, Int) -> (Extensive mv, Int) -> String
edgeLabel ((Discrete _ es), _) (t, _) = show $ edgeMove e
    where
        e = head $ filter ((== t) . edgeDest) es

makeEdge :: (Eq mv, Show mv) => (Extensive mv, Int) -> (Extensive mv, Int) -> DotEdge String
makeEdge n1 n2 = DotEdge (nodeId n1) (nodeId n2) [toLabel (edgeLabel n1 n2)]

makeEdges :: (Eq mv, Show mv) => [(Extensive mv, Int)] -> (Extensive mv, Int) -> [DotEdge String]
makeEdges nis ni = map (makeEdge ni) (nodesFrom nis ni)

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

