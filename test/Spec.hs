{-# LANGUAGE OverloadedStrings #-}

import           Control.Lens
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
      startFirst ^? gsBoard . at 1 . _Just . cPieces . at 1 `shouldBe` Just (Just 1)
    it "Lowers supply" $
      startFirst ^? gsPlayerStates . at 1 . _Just . psReserve `shouldBe` Just 7
    it "Second" $
      startSecond ^? gsBoard . at 1 . _Just . cPieces . at 2 `shouldBe` Just (Just 1)
    it "Starts next turn" $
      startSecond ^. gsCurrentTurn  `shouldBe` 3
  describe "Call Vote - No Help Needed" $ do
    it "Moves senator - adds" $
      noHelp ^? gsBoard . at 4 . _Just . cPieces . at 1 `shouldBe` Just (Just 1)
    it "Moves senator - removes" $
      noHelp ^? gsBoard . at 1 . _Just . cPieces . at 1 `shouldBe` Just Nothing
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
      needHelpFails ^? gsBoard . at 1 . _Just . cPieces . at 1 `shouldBe` Just (Just 1)
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
      needHelpSucceed ^? gsBoard . at 1 . _Just . cPieces . at 1 `shouldBe` Just (Just 0)
    it "Move Succeeded" $
      needHelpSucceed ^? gsBoard . at 8 . _Just . cPieces . at 1 `shouldBe` Just (Just 1)
    it "Got Laurel" $
      needHelpSucceed ^? gsPlayerStates . at 1 . _Just . psLaurels `shouldBe` Just [Laurel 2 False]
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
      caeserLaurel ^? gsBoard . at 1 . _Just . cPieces . at 1 `shouldBe` Just (Just 0)
    it "Move Succeeded" $
      caeserLaurel ^? gsBoard . at 5 . _Just . cPieces . at 1 `shouldBe` Just (Just 1)
    it "Caeser in hand" $
      caeserLaurel ^. gsPickedUpCaeser `shouldBe` True
    it "Not next turn" $
      caeserLaurel ^. gsCurrentTurn `shouldBe` 1
  describe "Move through Caeser" $ do
    it "Move Succeeded - left" $
      moveThroughCaeser ^? gsBoard . at 1 . _Just . cPieces . at 1 `shouldBe` Just (Just 0)
    it "Move Succeeded" $
      moveThroughCaeser ^? gsBoard . at 8 . _Just . cPieces . at 1 `shouldBe` Just (Just 1)
    it "Did not get laurel" $
      moveThroughCaeser ^? gsPlayerStates . at 1 . _Just . psLaurels `shouldBe` Just []
    it "Starts next turn" $
      moveThroughCaeser ^. gsCurrentTurn `shouldBe` 2

initGs = initialGameState defaultLaurels 5
  & gsPlayerStates . at 0 . _Just . psLaurels .~ [Laurel 4 False, Laurel 3 False]
  & gsEdges . at (Edge 1 5) . _Just .~ Just (Laurel 2 True)

makeMoves :: [Move] -> GameState
makeMoves = foldl' makeMove initGs

caesarMove = makeMoves [MoveCaeser (Edge 8 12)]
startFirst = makeMoves [StartSenator 1]
startSecond = makeMoves [StartSenator 1, StartSenator 1]
noHelp = makeMoves [StartSenator 0, StartSenator 1, StartSenator 1, StartSenator 1, StartSenator 1, CallVote 0 4]
needHelpFails = makeMoves [StartSenator 1, StartSenator 1, StartSenator 1, StartSenator 1, StartSenator 1, CallVote 1 8, Bribe 2 [Laurel 4 False], Bribe 3 [Laurel 3 False], CastVote 2 1, VoteOver]
needHelpSucceed = makeMoves [StartSenator 1, StartSenator 1, StartSenator 1, StartSenator 1, StartSenator 1, CallVote 1 8, CastVote 2 1, CastVote 3 1, VoteOver]
tooMuchSupport = makeMoves [StartSenator 1, StartSenator 1, StartSenator 1, StartSenator 1, StartSenator 1, CallVote 1 8, CastVote 2 1, CastVote 3 1, CastVote 4 1, VoteOver]
dispenseLaurels = makeMoves [StartSenator 1, StartSenator 1, StartSenator 1, StartSenator 1, StartSenator 1, CallVote 1 8, CastVote 2 1, CastVote 3 1, CastVote 4 1, VoteOver, DispenseSupportLaurel 2, DispenseSupportLaurel 3]
moveThroughCaeser = makeMoves [StartSenator 1, StartSenator 1, StartSenator 1, StartSenator 1, MoveCaeser (Edge 1 8), CallVote 1 8]
caeserLaurel = makeMoves [StartSenator 1, StartSenator 1, StartSenator 1, StartSenator 1, StartSenator 1, CallVote 1 5, CastVote 2 1, CastVote 3 1, VoteOver]
