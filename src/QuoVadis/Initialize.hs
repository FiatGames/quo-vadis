{-# LANGUAGE OverloadedStrings #-}

module QuoVadis.Initialize where

import           Control.Monad.Except
import qualified Data.HashMap.Strict  as M
import qualified Data.List            as L
import           QuoVadis.Types
import           System.Random

initialPlayerState :: PlayerState
initialPlayerState = PlayerState
  { _psReserve= 8
  , _psLaurels= []
  }

initialEdges :: [Laurel] -> ([Laurel], LabeledEdges)
initialEdges (l1:l2:l3:l4:l5:l6:l7:l8:l9:l10:ls) = (ls, M.fromList
  [ (Edge 0 4,Nothing)
  , (Edge 4 8,Nothing)
  , (Edge 8 12,Just l1)
  , (Edge 12 13,Just l2)
  , (Edge 1 8,Just l3)
  , (Edge 1 5,Just l4)
  , (Edge 5 9,Nothing)
  , (Edge 9 13,Just l5)
  , (Edge 2 6,Nothing)
  , (Edge 6 9,Just l6)
  , (Edge 6 11,Just l7)
  , (Edge 11 14,Nothing)
  , (Edge 14 13,Just l8)
  , (Edge 3 6,Just l9)
  , (Edge 3 7,Just l10)
  , (Edge 7 10,Nothing)
  , (Edge 10 14,Nothing)
  ])
initialEdges _ = error "Bad number of laurels"

initialBoardState :: Board
initialBoardState = M.fromList
  [ (0,Committee 1 mempty)
  , (1,Committee 5 mempty)
  , (2,Committee 1 mempty)
  , (3,Committee 3 mempty)
  , (4,Committee 1 mempty)
  , (5,Committee 1 mempty)
  , (6,Committee 3 mempty)
  , (7,Committee 1 mempty)
  , (8,Committee 3 mempty)
  , (9,Committee 5 mempty)
  , (10,Committee 1 mempty)
  , (11,Committee 1 mempty)
  , (12,Committee 3 mempty)
  , (13,Committee 5 mempty)
  , (14,Committee 3 mempty)
  ]

defaultLaurels :: [Laurel]
defaultLaurels = concat
  [ replicate 8 (Laurel 2 False)
  , replicate 15 (Laurel 3 False)
  , replicate 9 (Laurel 4 False)
  , replicate 6 (Laurel 5 False)
  , replicate 6 (Laurel 2 True)
  ]

type NumPlayers = Int

initialGameState :: [Laurel] -> Rivals -> GameState
initialGameState reserve rivals = GameState
  { _gsPlayerStates = M.fromList $ map (\p -> (p, initialPlayerState)) [1..length rivals]
  , _gsLaurelReserve = reserve'
  , _gsBoard = initialBoardState
  , _gsEdges = edges
  , _gsILaurelReserve = replicate 18 (Laurel 1 False)
  , _gsCaeser = Edge 9 13
  , _gsCurrentTurn = 1
  , _gsBribes = mempty
  , _gsVotes = mempty
  , _gsInProgressVote = Nothing
  , _gsPickedUpACaeserLaurel = False
  , _gsWinners = Nothing
  , _gsLaurelsToDispense = mempty
  , _gsRivals = rivals
  }
  where (reserve', edges) = initialEdges reserve

default5Player :: GameState
default5Player = initialGameState defaultLaurels (M.fromList [(1,2), (2,3), (3,4), (4,5), (5,1)])

randomInitialState :: (MonadIO m) => NumPlayers -> m GameState
randomInitialState ps = do
  rivals <- liftIO $ randomRival [1..ps]
  laurels <- liftIO $ randomShuffle defaultLaurels
  pure $ initialGameState laurels (M.fromList rivals)

randomShuffle :: (MonadIO m) => [a] -> m [a]
randomShuffle = go
  where
    go [] = pure []
    go xs = do
      i <- liftIO $ randomRIO (0,length xs - 1)
      let (hs,ts)
            | i == 0 = ([],tail xs)
            | i == length xs - 1 = (init xs,[])
            | otherwise = let (hs', ts') = L.splitAt i xs in (hs', tail ts')
      (:) (xs !! i) <$> go (hs ++ ts)

randomRival :: (MonadIO m) => [Player] -> m [(Player,Player)]
randomRival xs = go (xs,xs)
  where
    go ([p],[r])     = pure [(p,r)]
    go (p:ps,rs) = do
      rrs <- randomShuffle $ L.delete p rs
      if null rrs
      then pure []
      else do
        let rs' = if p `L.elem` rs then p:tail rrs else tail rrs
        (:) (p,head rrs) <$> go (ps,rs')
    go _ = pure []
