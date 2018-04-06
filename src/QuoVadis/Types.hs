{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

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
import           GHC.Generics

data Laurel = Laurel
  { _lScore    :: !Int
  , _lIsCaeser :: !Bool
  }
  deriving (Eq, Show, Generic)
makeLenses ''Laurel

type Player = Int

type NumSenators = Int

data Committee = Committee
  { _cNum    :: !Int
  , _cPieces :: !(HashMap Player NumSenators)
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
  { _psReserve :: !NumSenators
  , _psLaurels :: ![Laurel]
  }
  deriving (Eq, Show, Generic)
makeLenses ''PlayerState

type NumVotes = Int

data GameState = GameState
  { _gsPlayerStates   :: !(HashMap Player PlayerState)
  , _gsLaurelReserve  :: ![Laurel]
  , _gsBoard          :: !Board
  , _gsEdges          :: !LabeledEdges
  , _gsILaurelReserve :: ![Laurel]
  , _gsCaeser         :: Edge
  , _gsCurrentTurn    :: Player
  , _gsVotes          :: !(HashMap Player NumVotes)
  , _gsBribes         :: !(HashMap Player [Laurel])
  , _gsInProgressVote :: Maybe Edge
  }
  deriving (Eq, Show, Generic)
makeLenses ''GameState

data Move
  = MoveCaeser Edge
  | StartSenator BoardSpace
  | CallVote BoardSpace BoardSpace
  | CastVote Player NumVotes
  | Bribe Player [Laurel]
  | VoteOver

initialPlayerState :: PlayerState
initialPlayerState = PlayerState
  { _psReserve= 8
  , _psLaurels= []
  }

canBringIntoGame :: Int -> Board -> Bool
canBringIntoGame x b
  | x == 0 || x == 1 || x == 2 || x == 3 = case b^.at x of
    Nothing -> False
    Just c  -> (c^.cNum) > getSum (foldMap Sum (c^.cPieces))
  | otherwise = False

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

initialGameState :: [Laurel] -> NumPlayers -> GameState
initialGameState reserve ps = GameState
  { _gsPlayerStates = M.fromList $ map (\p -> (p, initialPlayerState)) [0..ps]
  , _gsLaurelReserve = reserve'
  , _gsBoard = initialBoardState
  , _gsEdges = edges
  , _gsILaurelReserve = replicate 18 (Laurel 1 False)
  , _gsCaeser = Edge 9 13
  , _gsCurrentTurn = 0
  , _gsBribes = mempty
  , _gsVotes = mempty
  , _gsInProgressVote = Nothing
  }
  where (reserve', edges) = initialEdges reserve


makeMove :: GameState -> Move -> GameState
makeMove gs mv = flip execState gs $ case mv of
  MoveCaeser e -> do
    gsCaeser .= e
    nextTurn

  StartSenator s -> do
    currentTurn <- use gsCurrentTurn
    addToCommittee s
    gsPlayerStates . at currentTurn . _Just . psReserve %= flip (-) 1
    nextTurn

  CallVote f t -> do
      forSelf <- numForSelf f
      needed <- votesNeeded f
      caeser <- caeserEdge f t
      if forSelf >= needed || caeser
      then do
        moveSenator f t
        nextTurn
      else gsInProgressVote .= Just (Edge f t)

  CastVote p votes -> gsVotes %= M.alter (const $ if votes == 0 then Nothing else Just votes) p

  Bribe p ls -> gsBribes %= M.alter (const $ if null ls then Nothing else Just ls) p

  VoteOver -> do
    Just (Edge f t) <- use gsInProgressVote
    forSelf <- numForSelf f
    numFor <- (forSelf +) . getSum . foldMap Sum <$> use gsVotes
    needed <- votesNeeded f
    when (numFor >= needed) $ moveSenator f t
    mapM_ transferBribe $ M.toList $ gs ^. gsBribes
    gsBribes .= mempty
    gsVotes .= mempty
    gsInProgressVote .= Nothing
    nextTurn

  where
    transferBribe (p,b) = do
      currentTurn <- use gsCurrentTurn
      laurelsForPlayer currentTurn %= flip (L.\\) b
      laurelsForPlayer p %= (++) b

    laurelsForPlayer p = gsPlayerStates . at p . _Just . psLaurels

    moveSenator f t = do
      removeFromCommitte f
      addToCommittee t

    boardAt s = gsBoard . at s . _Just

    removeFromCommitte = changeCommittee (Just . maybe 0 (flip (-) 1))
    addToCommittee = changeCommittee (Just . maybe 1 (1 +))
    changeCommittee f s = do
      currentTurn <- use gsCurrentTurn
      (boardAt s . cPieces) %= M.alter f currentTurn

    numForSelf s = do
      currentTurn <- use gsCurrentTurn
      fromMaybe (0 :: Int) <$> preuse (boardAt s . cPieces . at currentTurn . _Just)

    votesNeeded s = do
      maxInC <- fromMaybe 0 <$> preuse (boardAt s . cNum)
      pure $ ceiling $ fromIntegral maxInC / 2

    nextTurn = do
      currentTurn <- use gsCurrentTurn
      numPlayers <- M.size <$> use gsPlayerStates
      gsCurrentTurn .= (currentTurn + 1) `mod` (numPlayers - 1)

    caeserEdge s1 s2 = (==) (Edge s1 s2) <$> use gsCaeser
