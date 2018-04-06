{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
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
  { _gsPlayerStates      :: !(HashMap Player PlayerState)
  , _gsLaurelReserve     :: ![Laurel]
  , _gsBoard             :: !Board
  , _gsEdges             :: !LabeledEdges
  , _gsILaurelReserve    :: ![Laurel]
  , _gsCaeser            :: !Edge
  , _gsCurrentTurn       :: !Player
  , _gsVotes             :: !(HashMap Player NumVotes)
  , _gsBribes            :: !(HashMap Player [Laurel])
  , _gsInProgressVote    :: !(Maybe Edge)
  , _gsLaurelsToDispense :: ![Laurel]
  , _gsPickedUpCaeser    :: !Bool
  , _gsGameOver          :: !Bool
  }
  deriving (Eq, Show, Generic)
makeLenses ''GameState

boardSpace s = gsBoard . at s . _Just
innerSanctum = boardSpace 13
edge f t = gsEdges . at (Edge f t) . _Just

data Move
  = MoveCaeser Edge
  | StartSenator BoardSpace
  | CallVote BoardSpace BoardSpace
  | CastVote Player NumVotes
  | Bribe Player [Laurel]
  | VoteOver
  | DispenseSupportLaurel Player

canBringIntoGame :: Int -> Board -> Bool
canBringIntoGame x b
  | x == 0 || x == 1 || x == 2 || x == 3 = case b^.at x of
    Nothing -> False
    Just c  -> (c^.cNum) > getSum (foldMap Sum (c^.cPieces))
  | otherwise = False


makeMove :: GameState -> Move -> GameState
makeMove gs mv = flip execState gs $ case mv of
  MoveCaeser e -> do
    gsCaeser .= e
    gsPickedUpCaeser .= False
    nextTurn

  StartSenator s -> do
    currentTurn <- use gsCurrentTurn
    addToCommittee s
    gsPlayerStates . at currentTurn . _Just . psReserve %= \r -> r - 1
    nextTurn

  CallVote f t -> do
      forSelf <- numForSelf f
      needed <- votesNeeded f
      caeser <- caeserEdge f t
      if forSelf >= needed || caeser
      then do
        moveSenator f t
        haveCaeser <- use gsPickedUpCaeser
        unless haveCaeser nextTurn
      else gsInProgressVote .= Just (Edge f t)

  CastVote p votes -> gsVotes %= M.alter (const $ if votes == 0 then Nothing else Just votes) p

  Bribe p ls -> gsBribes %= M.alter (const $ if null ls then Nothing else Just ls) p

  VoteOver -> do
    Just (Edge f t) <- use gsInProgressVote
    forSelf <- numForSelf f
    votesFromOthers <- use gsVotes
    let numVotesFromOthers = getSum (foldMap Sum votesFromOthers)
        totalVotes = forSelf + numVotesFromOthers
    needed <- votesNeeded f
    when (totalVotes >= needed) $ moveSenator f t
    when (totalVotes == needed) $ mapM_ giveSupportLaurel $ M.toList votesFromOthers
    when (totalVotes > needed) $ replicateM_ (needed - forSelf) $ dispenseLaurel gsILaurelReserve gsLaurelsToDispense (:)
    gsVotes .= mempty

    bribes <- use gsBribes
    mapM_ transferBribe $ M.toList bribes
    gsBribes .= mempty

    gsInProgressVote .= Nothing

    Just i <- preuse $ innerSanctum . cPieces
    when (M.size i == 5) $ gsGameOver .= True

    haveCaeser <- use gsPickedUpCaeser
    anyLaurelsToGive <- not . null <$> use gsLaurelsToDispense
    gameOver <- use gsGameOver
    unless (haveCaeser || anyLaurelsToGive || gameOver) nextTurn

  DispenseSupportLaurel p -> do
    dispenseLaurel gsLaurelsToDispense (gsPlayerStates . at p . _Just . psLaurels) (:)
    noMoreToGive <- null <$> use gsLaurelsToDispense
    when noMoreToGive nextTurn

  where
    transferBribe (p,b) = do
      currentTurn <- use gsCurrentTurn
      laurelsForPlayer currentTurn %= \ls -> (L.\\) ls b
      laurelsForPlayer p %= (++) b

    laurelsForPlayer p = gsPlayerStates . at p . _Just . psLaurels

    giveSupportLaurel (p,votes) = replicateM_ votes $ dispenseLaurel gsILaurelReserve (laurelsForPlayer p) (:)

    dispenseLaurel :: (Monad m, MonadState GameState m) => Lens' GameState [Laurel] -> ASetter' GameState (f Laurel) -> (Laurel -> f Laurel -> f Laurel) -> m ()
    dispenseLaurel f t with = do
      ls <- use f
      unless (null ls) $ do
        f .= tail ls
        t %= with (head ls)

    moveSenator f t = do
      currentTurn <- use gsCurrentTurn
      Just mL <- preuse $ edge f t
      caeser <- (==) (Edge f t) <$> use gsCaeser
      unless caeser $ do
        gsPlayerStates . at currentTurn . _Just . psLaurels %= (++) (fromMaybe [] $ pure <$> mL)
        gsPickedUpCaeser .= maybe False (view lIsCaeser) mL
        dispenseLaurel gsLaurelReserve (edge f t) $ const . Just
      removeFromCommitte f
      addToCommittee t

    removeFromCommitte = changeCommittee (Just . maybe 0 (flip (-) 1))
    addToCommittee = changeCommittee (Just . maybe 1 (1 +))
    changeCommittee f s = do
      currentTurn <- use gsCurrentTurn
      (boardSpace s . cPieces) %= M.alter f currentTurn

    numForSelf s = do
      currentTurn <- use gsCurrentTurn
      fromMaybe (0 :: Int) <$> preuse (boardSpace s . cPieces . at currentTurn . _Just)

    votesNeeded s = do
      maxInC <- fromMaybe 0 <$> preuse (boardSpace s . cNum)
      pure $ ceiling $ fromIntegral maxInC / 2

    nextTurn = do
      currentTurn <- use gsCurrentTurn
      numPlayers <- M.size <$> use gsPlayerStates
      gsCurrentTurn .= (currentTurn + 1) `mod` numPlayers

    caeserEdge s1 s2 = (==) (Edge s1 s2) <$> use gsCaeser
