{-# LANGUAGE ScopedTypeVariables #-}

module Hagl.More.Vis where

import Data.Maybe (fromMaybe)
import Data.List (nub
                , intercalate)

import Hagl

import Data.GraphViz
import Data.GraphViz.Attributes

type VisNode mv = ([mv], Extensive mv)
type VisEdge mv = (VisNode mv, VisNode mv)

visNodes :: Extensive mv -> [VisNode mv]
visNodes t = nodes [] t
    where
        nodes p t = (p, t) : concatMap (followEdge p) (dtreeEdges t)
        followEdge p (m, t) = nodes (m : p) t

visEdges :: (Eq mv) => Extensive mv -> [VisEdge mv]
visEdges t = [(a, b) | a <- visNodes t
                     , b <- visNodes t
                     , a `edge` b]
    where
        edge (as, _) (_:bs, _) = as == bs
        edge _ _ = False

nodeId :: (Show mv) => VisNode mv -> String
nodeId (ms, _) = concat $ map show ms

toDotNode :: (Show mv) => VisNode mv -> DotNode String
toDotNode n@(ms, t) = DotNode (nodeId n) [toLabel (nodeLabel t)]
    where
        nodeLabel = actionLabel . nodeAction . dtreeNode
        actionLabel (Decision i) = show i
        actionLabel (Chance d) = "Chance\n" ++ intercalate "\n" (map show d)
        actionLabel (Payoff (ByPlayer ps)) = show ps

toDotEdge :: (Show mv) => VisEdge mv -> DotEdge String
toDotEdge e@(na, nb) = DotEdge (nodeId na) (nodeId nb) [toLabel (edgeLabel e)]
    where
        edgeLabel (_, (ms, _)) = show $ head ms

extensiveToDot :: (Eq mv, Show mv) => Extensive mv -> DotGraph String
extensiveToDot g = DotGraph { strictGraph = False
                            , directedGraph = True
                            , graphID = Nothing
                            , graphStatements = DotStmts { attrStmts = []
                                                         , subGraphs = []
                                                         , nodeStmts = nodes
                                                         , edgeStmts = edges }}
    where
        nodes = map toDotNode (visNodes g)
        edges = map toDotEdge (visEdges g)


