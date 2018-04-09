{-# LANGUAGE OverloadedStrings #-}

import           Control.Lens
import qualified Data.HashMap.Strict as M
import           Data.List           (foldl')
import           QuoVadis.Initialize
import           QuoVadis.Types
import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Move Caeser" $ do
    it "Moves him" $
      caesarMove ^. gsCaeser  `shouldBe` Edge 8 12
    it "Starts next turn" $
      caesarMove ^. gsCurrentTurn  `shouldBe` 2
  describe "Start Senator" $ do
    it "First" $
      startFirst ^? gsBoard . at 1 . _Just . cPieces . at 1 `shouldBe` Just (Just [1])
    it "Lowers supply" $
      startFirst ^? gsPlayerStates . at 1 . _Just . psReserve `shouldBe` Just 7
    it "Second" $
      startSecond ^? gsBoard . at 1 . _Just . cPieces . at 2 `shouldBe` Just (Just [2])
    it "Starts next turn" $
      startSecond ^. gsCurrentTurn  `shouldBe` 3
  describe "Call Vote - No Help Needed" $ do
    it "Moves senator - adds" $
      noHelp ^? gsBoard . at 4 . _Just . cPieces . at 1 `shouldBe` Just (Just [1])
    it "Moves senator - removes" $
      noHelp ^? gsBoard . at 0 . _Just . cPieces . at 1 `shouldBe` Just (Just [])
    it "Starts next turn" $
      noHelp ^. gsCurrentTurn `shouldBe` 2
  describe "Call Vote - Fizzle" $ do
    it "Bribes cleared" $
      needHelpFails ^? gsBribes `shouldBe` Just mempty
    it "Votes cleared" $
      needHelpFails ^? gsVotes `shouldBe` Just mempty
    it "In progress vote cleared" $
      needHelpFails ^. gsInProgressVote `shouldBe` Nothing
    it "Bribe went through - bribee 1" $
      needHelpFails ^? gsPlayerStates . at 2 . _Just . psLaurels `shouldBe` Just [Laurel 4 False]
    it "Bribe went through - bribee 2" $
      needHelpFails ^? gsPlayerStates . at 3 . _Just . psLaurels `shouldBe` Just [Laurel 3 False]
    it "Bribe went through - briber" $
      needHelpFails ^? gsPlayerStates . at 1 . _Just . psLaurels `shouldBe` Just []
    it "Move Fizzled" $
      needHelpFails ^? gsBoard . at 1 . _Just . cPieces . at 1 `shouldBe` Just (Just [1])
    it "Starts next turn" $
      needHelpFails ^. gsCurrentTurn `shouldBe` 2
  describe "Call Vote - Success" $ do
    it "Bribes cleared" $
      needHelpSucceed ^? gsBribes `shouldBe` Just mempty
    it "Votes cleared" $
      needHelpSucceed ^? gsVotes `shouldBe` Just mempty
    it "In progress vote cleared" $
      needHelpSucceed ^. gsInProgressVote `shouldBe` Nothing
    it "Move Succeeded - left" $
      needHelpSucceed ^? gsBoard . at 1 . _Just . cPieces . at 1 `shouldBe` Just (Just [])
    it "Move Succeeded" $
      needHelpSucceed ^? gsBoard . at 8 . _Just . cPieces . at 1 `shouldBe` Just (Just [1])
    it "Got Laurel" $
      needHelpSucceed ^? gsPlayerStates . at 1 . _Just . psLaurels `shouldBe` Just [Laurel 2 False, Laurel 4 False, Laurel 3 False]
    it "Got Support Laurel - P2" $
      needHelpSucceed ^? gsPlayerStates . at 2 . _Just . psLaurels `shouldBe` Just [Laurel 1 False]
    it "Got Support Laurel - P3" $
      needHelpSucceed ^? gsPlayerStates . at 3 . _Just . psLaurels `shouldBe` Just [Laurel 1 False]
    it "Support Laurel Reserve Diminished" $
      length (needHelpSucceed ^. gsILaurelReserve) `shouldBe` 16
    it "Laurel Reserve Diminished" $
      length (needHelpSucceed ^. gsLaurelReserve) `shouldBe` 33
    it "Starts next turn" $
      needHelpSucceed ^. gsCurrentTurn `shouldBe` 2
  describe "Call Vote - Too many" $ do
    it "Support Laurel Reserve Diminished" $
      length (tooMuchSupport ^. gsILaurelReserve) `shouldBe` 16
    it "Laurels To Dispense Increased" $
      length (tooMuchSupport ^. gsLaurelsToDispense) `shouldBe` 2
    it "Still player 1 turn" $
      tooMuchSupport ^. gsCurrentTurn `shouldBe` 1
  describe "Call Vote - Dispense Support" $ do
    it "Support Laurel Reserve Diminished" $
      length (dispenseLaurels ^. gsILaurelReserve) `shouldBe` 16
    it "Laurels To Dispense Increased" $
      length (dispenseLaurels ^. gsLaurelsToDispense) `shouldBe` 0
    it "Got Support Laurel - P2" $
      dispenseLaurels ^? gsPlayerStates . at 2 . _Just . psLaurels `shouldBe` Just [Laurel 1 False]
    it "Got Support Laurel - P3" $
      dispenseLaurels ^? gsPlayerStates . at 3 . _Just . psLaurels `shouldBe` Just [Laurel 1 False]
    it "Starts next turn" $
      needHelpSucceed ^. gsCurrentTurn `shouldBe` 2
  describe "Caeser Laurel" $ do
    it "Move Succeeded - left" $
      caeserLaurel ^? gsBoard . at 1 . _Just . cPieces . at 1 `shouldBe` Just (Just [])
    it "Move Succeeded" $
      caeserLaurel ^? gsBoard . at 5 . _Just . cPieces . at 1 `shouldBe` Just (Just [1])
    it "Caeser in hand" $
      caeserLaurel ^. gsPickedUpCaeser `shouldBe` True
    it "Not next turn" $
      caeserLaurel ^. gsCurrentTurn `shouldBe` 1
  describe "Move through Caeser" $ do
    it "Move Succeeded - left" $
      moveThroughCaeser ^? gsBoard . at 1 . _Just . cPieces . at 1 `shouldBe` Just (Just [])
    it "Move Succeeded" $
      moveThroughCaeser ^? gsBoard . at 8 . _Just . cPieces . at 1 `shouldBe` Just (Just [1])
    it "Did not get laurel" $
      moveThroughCaeser ^? gsPlayerStates . at 1 . _Just . psLaurels `shouldBe` Just [Laurel 4 False, Laurel 3 False]
    it "Starts next turn" $
      moveThroughCaeser ^. gsCurrentTurn `shouldBe` 2
  describe "Moves for In Progress Vote" $ do
    it "CastVotes" $
      moves 2 voteInProgress `shouldBe` [CastVote 2 1]
  describe "Moves for picked up caeser laurel" $
    it "Move" $
      moves 1 caeserLaurel `shouldBe` [MoveCaeser (Edge {_eFrom = 8, _eTo = 12}),MoveCaeser (Edge {_eFrom = 12, _eTo = 13}),MoveCaeser (Edge {_eFrom = 1, _eTo = 8}),MoveCaeser (Edge {_eFrom = 1, _eTo = 5}),MoveCaeser (Edge {_eFrom = 9, _eTo = 13}),MoveCaeser (Edge {_eFrom = 6, _eTo = 9}),MoveCaeser (Edge {_eFrom = 6, _eTo = 11}),MoveCaeser (Edge {_eFrom = 14, _eTo = 13}),MoveCaeser (Edge {_eFrom = 3, _eTo = 6}),MoveCaeser (Edge {_eFrom = 3, _eTo = 7})]
  describe "Moves for dispensing laurels" $
    it "Move" $ do
      moves 1 tooMuchSupport `shouldBe` [DispenseSupportLaurel 2,DispenseSupportLaurel 3,DispenseSupportLaurel 4]
      moves 1 moreLaurelsToGive `shouldBe` [DispenseSupportLaurel 3,DispenseSupportLaurel 4]
  describe "Regular move" $ do
    it "FirstMove" $
      moves 1 initGs `shouldBe` [MoveCaeser (Edge {_eFrom = 8, _eTo = 12}),MoveCaeser (Edge {_eFrom = 12, _eTo = 13}),MoveCaeser (Edge {_eFrom = 1, _eTo = 8}),MoveCaeser (Edge {_eFrom = 1, _eTo = 5}),MoveCaeser (Edge {_eFrom = 9, _eTo = 13}),MoveCaeser (Edge {_eFrom = 6, _eTo = 9}),MoveCaeser (Edge {_eFrom = 6, _eTo = 11}),MoveCaeser (Edge {_eFrom = 14, _eTo = 13}),MoveCaeser (Edge {_eFrom = 3, _eTo = 6}),MoveCaeser (Edge {_eFrom = 3, _eTo = 7}),StartSenator 0,StartSenator 1,StartSenator 2,StartSenator 3]
    it "Can call vote" $
      moves 1 canCallVote `shouldBe` [MoveCaeser (Edge {_eFrom = 8, _eTo = 12}),MoveCaeser (Edge {_eFrom = 12, _eTo = 13}),MoveCaeser (Edge {_eFrom = 1, _eTo = 8}),MoveCaeser (Edge {_eFrom = 1, _eTo = 5}),MoveCaeser (Edge {_eFrom = 9, _eTo = 13}),MoveCaeser (Edge {_eFrom = 6, _eTo = 9}),MoveCaeser (Edge {_eFrom = 6, _eTo = 11}),MoveCaeser (Edge {_eFrom = 14, _eTo = 13}),MoveCaeser (Edge {_eFrom = 3, _eTo = 6}),MoveCaeser (Edge {_eFrom = 3, _eTo = 7}), CallVote (Edge 1 5), CallVote (Edge 1 8), CallVote (Edge 3 6), CallVote (Edge 3 7)]

initGs = initialGameState defaultLaurels (M.fromList [(1,2), (2,3), (3,4), (4,5), (5,1)]) 5
  & gsPlayerStates . at 1 . _Just . psLaurels .~ [Laurel 4 False, Laurel 3 False]
  & gsEdges . at (Edge 1 5) . _Just .~ Just (Laurel 2 True)

makeTestMoves :: [Move] -> GameState
makeTestMoves = foldl' makeMove initGs

caesarMove = makeTestMoves [MoveCaeser (Edge 8 12)]
startFirst = makeTestMoves [StartSenator 1]
startSecond = makeTestMoves [StartSenator 1, StartSenator 1]
noHelp = makeTestMoves [StartSenator 0, StartSenator 1, StartSenator 1, StartSenator 1, StartSenator 1, CallVote (Edge 0 4)]
needHelpFails = makeTestMoves [StartSenator 1, StartSenator 1, StartSenator 1, StartSenator 1, StartSenator 1, CallVote (Edge 1 8), Bribe 1 2 [Laurel 4 False], Bribe 1 3 [Laurel 3 False], CastVote 2 1, VoteOver]
needHelpSucceed = makeTestMoves [StartSenator 1, StartSenator 1, StartSenator 1, StartSenator 1, StartSenator 1, CallVote (Edge 1 8), CastVote 2 1, CastVote 3 1, VoteOver]
tooMuchSupport = makeTestMoves [StartSenator 1, StartSenator 1, StartSenator 1, StartSenator 1, StartSenator 1, CallVote (Edge 1 8), CastVote 2 1, CastVote 3 1, CastVote 4 1, VoteOver]
dispenseLaurels = makeTestMoves [StartSenator 1, StartSenator 1, StartSenator 1, StartSenator 1, StartSenator 1, CallVote (Edge 1 8), CastVote 2 1, CastVote 3 1, CastVote 4 1, VoteOver, DispenseSupportLaurel 2, DispenseSupportLaurel 3]
moveThroughCaeser = makeTestMoves [StartSenator 1, StartSenator 1, StartSenator 1, StartSenator 1, MoveCaeser (Edge 1 8), CallVote (Edge 1 8)]
caeserLaurel = makeTestMoves [StartSenator 1, StartSenator 1, StartSenator 1, StartSenator 1, StartSenator 1, CallVote (Edge 1 5), CastVote 2 1, CastVote 3 1, VoteOver]

voteInProgress = makeTestMoves [StartSenator 1, StartSenator 1, StartSenator 1, StartSenator 1, StartSenator 1, CallVote (Edge 1 8)]
moreLaurelsToGive = makeTestMoves [StartSenator 1, StartSenator 1, StartSenator 1, StartSenator 1, StartSenator 1, CallVote (Edge 1 8), CastVote 2 1, CastVote 3 1, CastVote 4 1, VoteOver, DispenseSupportLaurel 2]
canCallVote = makeTestMoves [StartSenator 1, StartSenator 1, StartSenator 1, StartSenator 1, StartSenator 1, StartSenator 3, StartSenator 3, StartSenator 3, StartSenator 2, StartSenator 0]
