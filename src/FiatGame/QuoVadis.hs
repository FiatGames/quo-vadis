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
      { playerStates = qgs ^. Q.gsPlayerStates . to toList
      , laurelReserve = qgs ^. Q.gsLaurelReserve
      , board = fmap (over _2 (view committeIso)) $ qgs ^. Q.gsBoard . to toList
      , edges = qgs ^. Q.gsEdges . to toList
      , iLaurelReserve = qgs ^. Q.gsILaurelReserve
      , caeser = qgs ^. Q.gsCaeser
      , currentTurn = qgs ^. Q.gsCurrentTurn
      , votes =  qgs ^. Q.gsVotes . to toList
      , bribes = fmap (over _2 toList) $ qgs ^. Q.gsBribes . to toList
      , inProgressVote = qgs ^. Q.gsInProgressVote
      , laurelsToDispense = qgs ^. Q.gsLaurelsToDispense
      , pickedUpACaeserLaurel = qgs ^. Q.gsPickedUpACaeserLaurel
      , winners =  qgs ^. Q.gsWinners
      , rivals = qgs ^. Q.gsRivals . to toList
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
  { cPlayerState = head $ playerStates gs ^.. traverse . filtered ((==) pl . fst) . _2
  , cBoard = board gs
  , cEdges = edges gs
  , cCaeser = caeser gs
  , cCurrentTurn = currentTurn gs
  , cVotes = head $ votes gs ^.. traverse . filtered ((==) pl . fst) . _2
  , cBribes = head $ bribes gs ^.. traverse . filtered ((==) pl . fst) . _2
  , cInProgressVote = inProgressVote gs
  , cLaurelsToDispense = laurelsToDispense gs
  , cPickedUpACaeserLaurel = pickedUpACaeserLaurel gs
  , cWinners = winners gs
  , cRival = undefined
  , cMoves = Q.moves pl $ gs ^. from gameStateIso
  }
