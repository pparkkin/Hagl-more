module Hagl.More.Vis where

import Data.Maybe (fromMaybe)
import Data.List.Utils (flipAL)

import Hagl

import Data.GraphViz
import Data.GraphViz.Attributes
import Data.GraphViz.Commands

-- For example
-- TODO: Remove!
import Hagl.Examples.Crisis

extensiveNodes :: Extensive mv -> [Extensive mv]
extensiveNodes = bfs

zipWithIndex :: [a] -> [(Int, a)]
zipWithIndex = zip [0..]

extensiveEdge :: Extensive mv -> ExtEdge mv -> (Extensive mv, Extensive mv)
extensiveEdge n (_, n') = (n, n')

extensiveEdges :: Extensive mv -> [(Extensive mv, Extensive mv)]
extensiveEdges (Discrete _ []) = []
extensiveEdges n@(Discrete a es) = map (n `extensiveEdge`) es

nodesFrom :: (Eq mv) => [(Int, Extensive mv)] -> (Int, Extensive mv) -> [(Int, Extensive mv)]
nodesFrom _ (_, (Discrete _ [])) = []
nodesFrom nis (_, (Discrete _ es)) = map (index . snd) es
    where
        index n = (head $ fromMaybe [-1] $ lookup n $ flipAL nis, n)

nodeId :: (Int, Extensive mv) -> String
nodeId = show . fst

makeNode :: (Int, Extensive mv) -> DotNode String
makeNode ni = DotNode (nodeId ni) []

edgeId :: (Int, Extensive mv) -> (Int, Extensive mv) -> String
edgeId n1 n2 = (nodeId n1) ++ " -> " ++ (nodeId n2)

makeEdge :: (Int, Extensive mv) -> (Int, Extensive mv) -> DotEdge String
makeEdge n1 n2 = DotEdge (nodeId n1) (nodeId n2) [toLabel (edgeId n1 n2)]

makeEdges :: (Eq mv) => [(Int, Extensive mv)] -> (Int, Extensive mv) -> [DotEdge String]
makeEdges nis ni = map (makeEdge ni) (nodesFrom nis ni)

extensiveToDot :: (Eq mv) => Extensive mv -> DotGraph String
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

