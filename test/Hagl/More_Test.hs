
module Hagl.More_Test where

import Test.HUnit
import Test.Framework.Providers.HUnit

import Hagl
import Hagl.More

haglMoreTests = [
   testCase "Dominant strategies in prisoners' dilemma for player 1" testPDDSP1
 , testCase "Dominant strategies in prisoners' dilemma for player 2" testPDDSP2
 , testCase "Dominant strategies in matching pennies for player 1" testMPDSP1
 , testCase "Dominant strategies in matching pennies for player 2" testMPDSP2
 , testCase "Dominant strategies in cooperation game for player 1" testCGDSP1
 , testCase "Dominant strategies in cooperation game for player 2" testCGDSP2
 , testCase "Dominant strategies in battle of the sexes for player 1" testBSDSP1
 , testCase "Dominant strategies in battle of the sexes for player 2" testBSDSP2
 , testCase "Dominant strategies in dominance for player 1" testDDSP1
 , testCase "Dominant strategies in dominance for player 2" testDDSP2
 , testCase "Dominant strategies in voting for player 1" testVDSP1
 , testCase "Dominant strategies in voting for player 2" testVDSP2
 , testCase "Dominant strategies in voting for player 3" testVDSP3
 ]

testPDDSP1 = [Defect] @=? (dominantStrategies prisonersDilemma 1)
testPDDSP2 = [Defect] @=? (dominantStrategies prisonersDilemma 2)
testMPDSP1 = [] @=? (dominantStrategies matchingPennies 1)
testMPDSP2 = [] @=? (dominantStrategies matchingPennies 2)
testCGDSP1 = [] @=? (dominantStrategies cooperationGame 1)
testCGDSP2 = [] @=? (dominantStrategies cooperationGame 2)
testBSDSP1 = [] @=? (dominantStrategies battleOfTheSexes 1)
testBSDSP2 = [] @=? (dominantStrategies battleOfTheSexes 2)
testDDSP1 = [C] @=? (dominantStrategies dominance 1)
testDDSP2 = [Y] @=? (dominantStrategies dominance 2)
testVDSP1 = [V] @=? (dominantStrategies voting 1)
testVDSP2 = [O] @=? (dominantStrategies voting 2)
testVDSP3 = [O] @=? (dominantStrategies voting 3)

-- Some Test Games

data Dilemma = Cooperate | Defect deriving (Show, Eq)

prisonersDilemma :: Normal Dilemma
prisonersDilemma = normalGame [Cooperate, Defect]
                              [[-1, -1], [-4,  0],
                               [ 0, -4], [-3, -3]]

data Face = Heads | Tails deriving (Show, Eq)

matchingPennies :: Normal Face
matchingPennies = normalGame [Heads, Tails]
                             [[1, -1], [-1, 1],
                              [-1, 1], [1, -1]]

data Side = LeftSide | RightSide deriving (Show, Eq)

cooperationGame :: Normal Side
cooperationGame = normalGame [LeftSide, RightSide]
                             [[1, 1], [0, 0],
                              [0, 0], [1, 1]]

data Event = Football | Opera deriving (Show, Eq)

battleOfTheSexes :: Normal Event
battleOfTheSexes = normalGame [Football, Opera]
                              [[2, 1], [0, 0],
                               [0, 0], [1, 2]]

data PMove = A | B | C | D | X | Y | Z deriving (Show, Eq)

dominance :: Normal PMove
dominance = normal 2 [[A, B, C, D], [X, Y, Z]]
                     [[1, 2], [2, 2], [5, 1],
                      [4, 1], [3, 5], [3, 3],
                      [5, 2], [4, 4], [7, 0],
                      [2, 3], [0, 4], [3, 0]]

data Vote = V | O deriving (Show, Eq)

voting :: Normal Vote
voting = normal 3 [[V, O], [V, O], [V, O]]
                  [[1, 0, 0], [1, 0, 0],
                   [1, 0, 0], [0, 1, 1],
                   [1, 0, 0], [0, 1, 1],
                   [0, 1, 1], [0, 1, 1]]

-- Helper
normalGame :: [mv] -> [[Float]] -> Normal mv
normalGame mvs = normal 2 [mvs, mvs]
