
module Hagl.More_Test where

import Test.HUnit
import Test.Framework.Providers.HUnit

import Hagl
import Hagl.More.DominantStrategy
import Hagl.More.PureStrategies

haglMoreTests = [
   testCase "Pure strategies in prisoners' dilemma for player 1" testPDPSP1
 , testCase "Pure strategies in prisoners' dilemma for player 2" testPDPSP2
 , testCase "Pure strategies in matching pennies for player 1" testMPPSP1
 , testCase "Pure strategies in matching pennies for player 2" testMPPSP2
 , testCase "Pure strategies in cooperation game for player 1" testCGPSP1
 , testCase "Pure strategies in cooperation game for player 2" testCGPSP2
 , testCase "Pure strategies in battle of the sexes for player 1" testBSPSP1
 , testCase "Pure strategies in battle of the sexes for player 2" testBSPSP2
 , testCase "Pure strategies in dominance for player 1" testDPSP1
 , testCase "Pure strategies in dominance for player 2" testDPSP2
 , testCase "Pure strategies in voting for player 1" testVPSP1
 , testCase "Pure strategies in voting for player 2" testVPSP2
 , testCase "Pure strategies in voting for player 2" testVPSP3

 , testCase "Dominant strategies in prisoners' dilemma for player 1" testPDDSP1
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

 , testCase "Pure strategies in revolution for player 1" testRPSP1
 , testCase "Pure strategies in revolution for player 2" testRPSP2
 , testCase "Pure strategies in oversight for player 1" testOPSP1
 , testCase "Pure strategies in oversight for player 2" testOPSP2
 ]

testPDPSP1 = [[Cooperate], [Defect]] @=? (pureStrategies prisonersDilemma 1)
testPDPSP2 = [[Cooperate], [Defect]] @=? (pureStrategies prisonersDilemma 2)
testMPPSP1 = [[Heads], [Tails]] @=? (pureStrategies matchingPennies 1)
testMPPSP2 = [[Heads], [Tails]] @=? (pureStrategies matchingPennies 2)
testCGPSP1 = [[LeftSide], [RightSide]] @=? (pureStrategies cooperationGame 1)
testCGPSP2 = [[LeftSide], [RightSide]] @=? (pureStrategies cooperationGame 2)
testBSPSP1 = [[Football], [Opera]] @=? (pureStrategies battleOfTheSexes 1)
testBSPSP2 = [[Football], [Opera]] @=? (pureStrategies battleOfTheSexes 2)
testDPSP1 = [[A], [B], [C], [D]] @=? (pureStrategies dominance 1)
testDPSP2 = [[X], [Y], [Z]] @=? (pureStrategies dominance 2)
testVPSP1 = [[V], [O]] @=? (pureStrategies voting 1)
testVPSP2 = [[V], [O]] @=? (pureStrategies voting 2)
testVPSP3 = [[V], [O]] @=? (pureStrategies voting 3)

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

testRPSP1 = [["Consent"],["Revolt"]] @=? (pureStrategies revolution 1)
testRPSP2 = [["Eliminate tax","Suppress revolution"],
             ["Eliminate tax","Grant independence"],
             ["Tax","Suppress revolution"],
             ["Tax","Grant independence"]] @=? (pureStrategies revolution 2)
testOPSP1 = [["Low regulatory enforcement"],
             ["High regulatory enforcement"]] @=? (pureStrategies oversight 1)
testOPSP2 = [["No oversight","No oversight"],
             ["No oversight","Oversight"],
             ["Oversight","No oversight"],
             ["Oversight","Oversight"]] @=? (pureStrategies oversight 2)

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

revolution :: Extensive String
revolution = start
    where
      -- A is a colony controlled by B.
      a = player 1
      -- Country B generates revenue from control of A’s oil fields and from
      -- direct taxes on A’s residents.
      b = player 2

      -- In the event of war, A wins with probability p.
      aWins = 2
      bWins = 3

      rg = pays [3, 0]
      rs = chance
           [(aWins, "A wins war"), (bWins, "B wins war")]
           [("A wins war", pays [4 - 6, -6]), ("B wins war", pays [-6 - 2, 6 - 6])]
      ct = pays [-2, 6]
      ce = pays [0, 4]

      start = a ("Revolt", bGrantOrSuppress)
              <|> ("Consent", bTaxOrEliminateTax)

      bGrantOrSuppress = b ("Grant independence", rg)
                         <|> ("Suppress revolution", rs)

      bTaxOrEliminateTax = b ("Tax", ct)
                           <|> ("Eliminate tax", ce)

oversight :: Extensive String
oversight = start
    where
      b = player 1 -- bureaucrat
      p = player 2 -- politician

      c = 2 -- enforcement cost
      k = 1/2 -- oversight cost
      f = 4 -- penalty

      ho = pays [-c, 1-k] -- high enforcement/oversight
      hn = pays [-c, 1] -- high enforcement/no oversight
      lo = pays [-f-c, 1-k] -- low enforcement/oversight
      ln = pays [0, 0] -- low enforcement/no oversight

      start = b ("High regulatory enforcement", pH)
              <|> ("Low regulatory enforcement", pL)

      pH = p ("Oversight", ho)
           <|> ("No oversight", hn)

      pL = p ("Oversight", lo)
           <|> ("No oversight", ln)

-- Helper
normalGame :: [mv] -> [[Float]] -> Normal mv
normalGame mvs = normal 2 [mvs, mvs]
