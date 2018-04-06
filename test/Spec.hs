{-# LANGUAGE OverloadedStrings #-}

import           Control.Lens
import           Data.List      (foldl')
import           QuoVadis.Types
import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Move Caeser" $ do
    it "Moves him" $
      caesarMove ^. gsCaeser  `shouldBe` Edge 8 12
    it "Starts next turn" $
      caesarMove ^. gsCurrentTurn  `shouldBe` 1
  describe "Start Senator" $ do
    it "First" $
      startFirst ^? gsBoard . at 1 . _Just . cPieces . at 0 `shouldBe` Just (Just 1)
    it "Lowers supply" $
      startFirst ^? gsPlayerStates . at 0 . _Just . psReserve `shouldBe` Just 7
    it "Second" $
      startSecond ^? gsBoard . at 1 . _Just . cPieces . at 1 `shouldBe` Just (Just 1)
    it "Starts next turn" $
      startSecond ^. gsCurrentTurn  `shouldBe` 2
  describe "Call Vote - No Help Needed" $ do
    it "Moves senator - adds" $
      noHelp ^? gsBoard . at 4 . _Just . cPieces . at 0 `shouldBe` Just (Just 1)
    it "Moves senator - removes" $
      noHelp ^? gsBoard . at 1 . _Just . cPieces . at 0 `shouldBe` Just Nothing
    it "Starts next turn" $
      noHelp ^. gsCurrentTurn `shouldBe` 1
  describe "Call Vote - Fizzle" $ do
    it "Bribes cleared" $
      needHelpFails ^? gsBribes `shouldBe` Just mempty
    it "Votes cleared" $
      needHelpFails ^? gsVotes `shouldBe` Just mempty
    it "In progress vote cleared" $
      needHelpFails ^. gsInProgressVote `shouldBe` Nothing
    it "Bribe went through - bribee 1" $
      needHelpFails ^? gsPlayerStates . at 1 . _Just . psLaurels `shouldBe` Just [Laurel 4 False]
    it "Bribe went through - bribee 2" $
      needHelpFails ^? gsPlayerStates . at 2 . _Just . psLaurels `shouldBe` Just [Laurel 3 False]
    it "Bribe went through - briber" $
      needHelpFails ^? gsPlayerStates . at 0 . _Just . psLaurels `shouldBe` Just []
    it "Move Fizzled" $
      needHelpFails ^? gsBoard . at 1 . _Just . cPieces . at 0 `shouldBe` Just (Just 1)
    it "Starts next turn" $
      needHelpFails ^. gsCurrentTurn `shouldBe` 1

initGs = initialGameState defaultLaurels 5
  & gsPlayerStates . at 0 . _Just . psLaurels .~ [Laurel 4 False, Laurel 3 False]

makeMoves :: [Move] -> GameState
makeMoves = foldl' makeMove initGs

caesarMove = makeMoves [MoveCaeser (Edge 8 12)]
startFirst = makeMoves [StartSenator 1]
startSecond = makeMoves [StartSenator 1, StartSenator 1]
noHelp = makeMoves [StartSenator 0, StartSenator 1, StartSenator 1, StartSenator 1, StartSenator 1, CallVote 0 4]
needHelpFails = makeMoves [StartSenator 1, StartSenator 1, StartSenator 1, StartSenator 1, StartSenator 1, CallVote 1 8, Bribe 1 [Laurel 4 False], Bribe 2 [Laurel 3 False], CastVote 1 1, VoteOver]
