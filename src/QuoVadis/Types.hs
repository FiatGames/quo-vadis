{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}

module QuoVadis.Types where

import           Control.Lens
import           Control.Monad
import           Control.Monad.State.Strict
import           Data.Hashable
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as M
import           Data.List                  as L
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set                   as S
import           GHC.Generics               (Generic)

data Laurel = Laurel
  { _lScore    :: !Int
  , _lIsCaeser :: !Bool
  }
  deriving (Eq, Show, Ord, Generic)
makeLenses ''Laurel

type Player = Int

type PlaceInCommittee = Int
data Committee = Committee
  { _cNum    :: !Int
  , _cPieces :: !(HashMap Player [PlaceInCommittee])
  }
  deriving (Eq, Show, Generic)
makeLenses ''Committee

type BoardSpace = Int

type Board = HashMap BoardSpace Committee

data Edge = Edge
  { _eFrom :: !BoardSpace
  , _eTo   :: !BoardSpace
  }
  deriving (Eq, Show, Ord, Generic)
makeLenses ''Edge

instance Hashable Edge where
  hashWithSalt s (Edge x y) = x + y + s

type LabeledEdges = HashMap Edge (Maybe Laurel)

data PlayerState = PlayerState
  { _psReserve :: !Int
  , _psLaurels :: ![Laurel]
  }
  deriving (Eq, Show, Generic)
makeLenses ''PlayerState

type NumVotes = Int

type Rivals = HashMap Player Player

data GameState = GameState
  { _gsPlayerStates          :: !(HashMap Player PlayerState)
  , _gsLaurelReserve         :: ![Laurel]
  , _gsBoard                 :: !Board
  , _gsEdges                 :: !LabeledEdges
  , _gsILaurelReserve        :: ![Laurel]
  , _gsCaeser                :: !Edge
  , _gsCurrentTurn           :: !Player
  , _gsVotes                 :: !(HashMap Player NumVotes)
  , _gsBribes                :: !(HashMap Player (HashMap Player [Laurel]))
  , _gsInProgressVote        :: !(Maybe Edge)
  , _gsLaurelsToDispense     :: ![Laurel]
  , _gsPickedUpACaeserLaurel :: !Bool
  , _gsWinners               :: !(Maybe [Player])
  , _gsRivals                :: !Rivals
  }
  deriving (Eq, Show, Generic)
makeLenses ''GameState

--- Lenses ---
boardSpace :: BoardSpace -> Traversal' GameState Committee
boardSpace s = gsBoard . at s . _Just

innerSanctum :: Traversal' GameState Committee
innerSanctum = boardSpace 13

edge :: Edge -> Traversal' GameState (Maybe Laurel)
edge e = gsEdges . at e . _Just

laurelsForPlayer :: Player -> Traversal' GameState [Laurel]
laurelsForPlayer p = gsPlayerStates . at p . _Just . psLaurels

senatorsInReserve :: Player -> Traversal' GameState Int
senatorsInReserve p = gsPlayerStates . at p . _Just . psReserve

senatorsAtSpot :: Player -> BoardSpace -> Traversal' GameState [PlaceInCommittee]
senatorsAtSpot p s = gsBoard . at s . _Just . cPieces . at p . _Just

--- Folds ---
spacesWithSenatorFor :: Player -> Fold GameState (BoardSpace, Committee)
spacesWithSenatorFor p = gsBoard . folded . withIndex . filtered (anyOf (_2 . cPieces . at p . _Just . to length) (> 0))

oldestSenator :: BoardSpace -> Player -> Fold GameState Int
oldestSenator s p = boardSpace s . cPieces . at p . _Just . to minimum

votesNeeded :: BoardSpace -> Fold GameState Int
votesNeeded s = boardSpace s . cNum . to (\m -> ceiling $ fromIntegral m / 2)

--- Helpers ---
pointsForPlayer :: GameState -> Player -> Int
pointsForPlayer gs p = sum $ gs ^.. gsPlayerStates . at p . _Just . psLaurels . traverse . lScore

spotsAvailable :: GameState -> BoardSpace -> Bool
spotsAvailable gs x = case gs ^. gsBoard . at x of
    Nothing -> False
    Just c  -> (c^.cNum) > sumOf (cPieces . traversed . to length) c

adjacentTo :: GameState -> BoardSpace -> [Edge]
adjacentTo gs s = filter (\(Edge f t) -> s == f && spotsAvailable gs t) $ M.keys $ gs ^. gsEdges

hasBeatenRival :: GameState -> Player -> Bool
hasBeatenRival gs p
  | myPoints == theirPoints = myOldest < theirOldest
  | otherwise = myPoints > theirPoints
  where
    myPoints = pointsForPlayer gs p
    myOldest = fromMaybe maxBound $ gs ^? oldestSenator 13 p
    myRival = fromMaybe p $ gs ^? gsRivals . at p . _Just
    theirPoints = pointsForPlayer gs myRival
    theirOldest = fromMaybe maxBound $ gs ^? oldestSenator 13 myRival

laurelEdges :: [Edge]
laurelEdges = [Edge 8 12, Edge 12 13, Edge 1 8, Edge 1 5, Edge 9 13, Edge 6 9, Edge 6 11, Edge 14 13, Edge 3 6, Edge 3 7]
