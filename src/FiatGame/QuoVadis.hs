{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module FiatGame.QuoVadis where

import           Control.Lens
import           Control.Monad.Except
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.Set             as S
import           Data.Text            (Text)
import           Data.Time.Clock
import           FiatGame.Class       as FiatGame
import qualified FiatGame.Types       as FiatGame
import           GHC.Exts
import           GHC.Generics         hiding (from, to)
import qualified QuoVadis.Initialize  as Q
import qualified QuoVadis.Moves       as Q
import qualified QuoVadis.Types       as Q

$(deriveJSON defaultOptions ''Q.Laurel)
$(deriveJSON defaultOptions ''Q.PlayerState)
$(deriveJSON defaultOptions ''Q.Edge)
$(deriveJSON defaultOptions ''Q.Move)

data Settings = Settings
  { players              :: ![FiatGame.FiatPlayer]
  , playerMap            :: ![(FiatGame.FiatPlayer,Q.Player)]
  , turnTimeMilliseconds :: !Integer
  }
  deriving (Eq, Show, Generic)
$(deriveJSON defaultOptions ''Settings)

data Committee = Committee
  { cNum    :: !Int
  , cPieces :: ![(Q.Player,[Q.PlaceInCommittee])]
  }
  deriving (Eq, Show, Generic)
$(deriveJSON defaultOptions ''Committee)

committeIso :: Iso' Q.Committee Committee
committeIso = iso fromQ toQ
  where
    fromQ (Q.Committee n p) = Committee n $ toList p
    toQ (Committee n p) = Q.Committee n $ fromList p

data GameState = GameState
  { playerStates          :: ![(Q.Player, Q.PlayerState)]
  , laurelReserve         :: ![Q.Laurel]
  , board                 :: ![(Q.BoardSpace, Committee)]
  , edges                 :: ![(Q.Edge,Maybe Q.Laurel)]
  , iLaurelReserve        :: ![Q.Laurel]
  , caeser                :: !Q.Edge
  , currentTurn           :: Q.Player
  , votes                 :: ![(Q.Player,Q.NumVotes)]
  , bribes                :: ![(Q.Player, [(Q.Player, [Q.Laurel])])]
  , inProgressVote        :: !(Maybe Q.Edge)
  , laurelsToDispense     :: ![Q.Laurel]
  , pickedUpACaeserLaurel :: !Bool
  , winners               :: !(Maybe [Q.Player])
  , rivals                :: ![(Q.Player,Q.Player)]
  }
  deriving (Eq, Show, Generic)
$(deriveJSON defaultOptions ''GameState)

gameStateIso :: Iso' Q.GameState GameState
gameStateIso = iso fromQ toQ
  where
    fromQ qgs = GameState
      { playerStates = toList $ qgs ^. Q.gsPlayerStates
      , laurelReserve = qgs ^. Q.gsLaurelReserve
      , board = toList $ fmap (view committeIso) $ qgs ^. Q.gsBoard
      , edges = qgs ^. Q.gsEdges . to toList
      , iLaurelReserve = qgs ^. Q.gsILaurelReserve
      , caeser = qgs ^. Q.gsCaeser
      , currentTurn = qgs ^. Q.gsCurrentTurn
      , votes =  qgs ^. Q.gsVotes . to toList
      , bribes = toList $ fmap toList $ qgs ^. Q.gsBribes
      , inProgressVote = qgs ^. Q.gsInProgressVote
      , laurelsToDispense = qgs ^. Q.gsLaurelsToDispense
      , pickedUpACaeserLaurel = qgs ^. Q.gsPickedUpACaeserLaurel
      , winners =  qgs ^. Q.gsWinners
      , rivals = toList $ qgs ^. Q.gsRivals
      }
    toQ gs = Q.GameState
      { Q._gsPlayerStates = fromList $ playerStates gs
      , Q._gsLaurelReserve = laurelReserve gs
      , Q._gsBoard = fmap (view (from committeIso)) $ fromList $ board gs
      , Q._gsEdges = fromList $ edges gs
      , Q._gsILaurelReserve = iLaurelReserve gs
      , Q._gsCaeser = caeser gs
      , Q._gsCurrentTurn = currentTurn gs
      , Q._gsVotes = fromList $ votes gs
      , Q._gsBribes = fmap fromList $ fromList $ bribes gs
      , Q._gsInProgressVote = inProgressVote gs
      , Q._gsLaurelsToDispense = laurelsToDispense gs
      , Q._gsPickedUpACaeserLaurel = pickedUpACaeserLaurel gs
      , Q._gsWinners = winners gs
      , Q._gsRivals = fromList $ rivals gs
      }

data ClientGameState = ClientGameState
  { cPlayerState           :: !Q.PlayerState
  , cBoard                 :: ![(Q.BoardSpace, Committee)]
  , cEdges                 :: ![(Q.Edge,Maybe Q.Laurel)]
  , cCaeser                :: !Q.Edge
  , cCurrentTurn           :: !Q.Player
  , cVotes                 :: !Q.NumVotes
  , cBribes                :: ![(Q.Player, [Q.Laurel])]
  , cInProgressVote        :: !(Maybe Q.Edge)
  , cLaurelsToDispense     :: ![Q.Laurel]
  , cPickedUpACaeserLaurel :: !Bool
  , cWinners               :: !(Maybe [Q.Player])
  , cRival                 :: !Q.Player
  , cMoves                 :: ![Q.Move]
  }
  deriving (Eq, Show, Generic)
$(deriveJSON defaultOptions ''ClientGameState)

mkClientGameState :: Q.Player -> GameState -> ClientGameState
mkClientGameState pl gs = ClientGameState
  { cPlayerState = getMyPiece $ playerStates gs
  , cBoard = board gs
  , cEdges = edges gs
  , cCaeser = caeser gs
  , cCurrentTurn = currentTurn gs
  , cVotes = getMyPiece $ votes gs
  , cBribes = getMyPiece $ bribes gs
  , cInProgressVote = inProgressVote gs
  , cLaurelsToDispense = laurelsToDispense gs
  , cPickedUpACaeserLaurel = pickedUpACaeserLaurel gs
  , cWinners = winners gs
  , cRival = getMyPiece $ rivals gs
  , cMoves = Q.moves pl $ gs ^. from gameStateIso
  }
  where
    getMyPiece :: [(Q.Player, b)] -> b
    getMyPiece = head . toListOf (traverse . filtered ((==) pl . fst) . _2)

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
