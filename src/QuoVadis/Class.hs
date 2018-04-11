{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module QuoVadis.Class where

import           Control.Lens
import           Control.Monad.Except
import qualified Data.Set             as S
import           Data.Text            (Text)
import           Data.Time.Clock
import           FiatGame.Class       as FiatGame
import           FiatGame.QuoVadis
import qualified FiatGame.Types       as FiatGame
import qualified QuoVadis.Initialize  as Q
import qualified QuoVadis.Moves       as Q
import qualified QuoVadis.Types       as Q

instance FiatGame Settings where
  type Move Settings = Q.Move
  type State Settings = GameState
  type ClientState Settings = ClientGameState
  type ClientSettings Settings = Settings

  defaultSettings :: (MonadIO m) => m Settings
  defaultSettings = pure $ Settings [] [] 0

  addPlayer :: (MonadIO m) => FiatGame.FiatPlayer -> Settings -> m (Maybe Settings)
  addPlayer p (Settings ps _ tt)
    | length ps >= 5 = pure Nothing
    | otherwise  = pure $ Just $ Settings (p:ps) [] tt

  initialGameState :: (MonadIO m) => Settings -> m (Either Text (Settings, FiatGame.GameState GameState Q.Move))
  initialGameState (Settings ps _ tt)
    | length ps < 2 || length ps > 5 = pure $ Left "Incorrect number of players"
    | otherwise = do
      rPs <- Q.randomShuffle [1..length ps]
      let s = Settings ps (zip ps rPs) tt
      gs <- Q.randomInitialState (length ps)
      pure $ Right (s,FiatGame.GameState FiatGame.Playing (gs^.gameStateIso) Nothing)

  makeMove :: (MonadIO m) => FiatGame.FiatPlayer -> Settings -> FiatGame.GameState GameState Q.Move -> Q.Move -> m (FiatGame.GameState GameState Q.Move)
  makeMove _ s (FiatGame.GameState _ g _) m = case g' ^.  Q.gsWinners of
      Just _ -> pure $  FiatGame.GameState FiatGame.Done (g'^.gameStateIso) Nothing
      Nothing -> do
        moveTime <- addUTCTime (fromInteger $ turnTimeMilliseconds s) <$> liftIO getCurrentTime
        let mv = case g' ^. Q.gsInProgressVote of
                  Nothing -> Q.Pass
                  Just _  -> Q.VoteOver
        pure $ FiatGame.GameState FiatGame.Playing (g'^.gameStateIso) $ Just $ FiatGame.FutureMove moveTime mv
    where
      g' = Q.makeMove (g^.from gameStateIso) m

  isPlayersTurn :: (MonadIO m) => FiatGame.FiatPlayer -> Settings -> FiatGame.GameState GameState Q.Move -> Q.Move -> m Bool
  isPlayersTurn p s g  _      = pure $ not $ null $ getAllMoves p s g

  isMoveValid :: (MonadIO m) => FiatGame.FiatPlayer -> Settings -> FiatGame.GameState GameState Q.Move -> Q.Move -> m Bool
  isMoveValid p s g m = pure $ any (bribeEq m) $ getAllMoves p s g

  toClientSettingsAndState :: (MonadIO m) => FiatGame.FiatPlayer -> Settings -> Maybe (FiatGame.GameState GameState Q.Move) -> m (Settings, Maybe (FiatGame.GameState ClientGameState Q.Move))
  toClientSettingsAndState _ s Nothing = pure (s, Nothing)
  toClientSettingsAndState p s (Just (FiatGame.GameState st gs mv)) = pure (s, Just $ FiatGame.GameState st (mkClientGameState (getQPlayer s p) gs) mv)

bribeEq :: Q.Move -> Q.Move -> Bool
bribeEq (Q.Bribe p11 p12 ls1) (Q.Bribe p21 p22 ls2)
  = p11 == p21 && p12 == p22 && S.fromList ls1 == S.fromList ls2
bribeEq m1 m2 = m1 == m2

getAllMoves :: FiatGame.FiatPlayer -> Settings -> FiatGame.GameState GameState m -> [Q.Move]
getAllMoves p s (FiatGame.GameState _ g _)
  = Q.bribes pl (g ^. from gameStateIso) ++ Q.moves pl (g ^. from gameStateIso)
      where pl = getQPlayer s p

getQPlayer :: Settings -> FiatGame.FiatPlayer -> Q.Player
getQPlayer s p = snd $ head $ filter ((==) p . fst) $ playerMap s
