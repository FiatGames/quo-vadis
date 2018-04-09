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
  , _gsWinners           :: !(Maybe [Player])
  , _gsRivals            :: !Rivals
  }
  deriving (Eq, Show, Generic)
makeLenses ''GameState

data Move
  = MoveCaeser Edge
  | StartSenator BoardSpace
  | CallVote Edge
  | CastVote Player NumVotes
  | Bribe Player [Laurel]
  | VoteOver
  | DispenseSupportLaurel Player
  deriving (Eq, Show)

--- Lenses ---
boardSpace :: BoardSpace -> Traversal' GameState Committee
boardSpace s = gsBoard . at s . _Just

innerSanctum :: Traversal' GameState Committee
innerSanctum = boardSpace 13

edge :: Edge -> Traversal' GameState (Maybe Laurel)
edge e = gsEdges . at e . _Just

spacesWithSenatorFor :: Player -> Fold GameState (Int, Committee)
spacesWithSenatorFor p = gsBoard . folded . withIndex . filtered (anyOf (_2 . cPieces . at p . _Just . to length) (> 0))

--- State Helpers ---
points :: MonadState GameState m => Player -> m Int
points p = sum <$> gets (toListOf (gsPlayerStates . at p . _Just . psLaurels . traverse . lScore))

oldestSenator :: MonadState GameState m => BoardSpace -> Player -> m PlaceInCommittee
oldestSenator s p = maybe 0 minimum <$> preuse (boardSpace s . cPieces . at p . _Just)

dispenseLaurel :: MonadState GameState m => Lens' GameState [Laurel] -> ASetter' GameState (f Laurel) -> (Laurel -> f Laurel -> f Laurel) -> m ()
dispenseLaurel f t with = do
  ls <- use f
  unless (null ls) $ do
    f .= tail ls
    t %= with (head ls)

spotsAvailable :: Board -> Int -> Bool
spotsAvailable b x = case b^.at x of
    Nothing -> False
    Just c  -> (c^.cNum) > getSum (foldMap (Sum . length) (c^.cPieces))

adjacentTo :: GameState -> BoardSpace -> [Edge]
adjacentTo gs s = filter (\(Edge f t) -> s == f && spotsAvailable (gs ^. gsBoard) t) $ M.keys $ gs ^. gsEdges

makeMove :: GameState -> Move -> GameState
makeMove gs mv = flip execState gs $ case mv of
  MoveCaeser e -> do
    gsCaeser .= e
    gsPickedUpCaeser .= False
    goToNextTurn

  StartSenator s -> do
    currentTurn <- use gsCurrentTurn
    addToCommittee s
    gsPlayerStates . at currentTurn . _Just . psReserve %= \r -> r - 1
    goToNextTurn

  CallVote e -> do
      forSelf <- numForSelf $ e ^. eFrom
      needed <- votesNeeded $ e ^. eFrom
      caeser <- caeserEdge e
      if forSelf >= needed || caeser
      then do
        moveSenator e
        haveCaeser <- use gsPickedUpCaeser
        unless haveCaeser nextTurn
      else gsInProgressVote .= Just e

  CastVote p votes -> gsVotes %= M.alter (const $ if votes == 0 then Nothing else Just votes) p

  Bribe p ls -> gsBribes %= M.alter (const $ if null ls then Nothing else Just ls) p

  VoteOver -> do
    --Figure out votes
    Just (Edge f t) <- use gsInProgressVote
    forSelf <- numForSelf f
    votesFromOthers <- use gsVotes
    let numVotesFromOthers = getSum (foldMap Sum votesFromOthers)
        totalVotes = forSelf + numVotesFromOthers
    needed <- votesNeeded f
    when (totalVotes >= needed) $ moveSenator (Edge f t)
    when (totalVotes == needed) $ mapM_ giveSupportLaurel $ M.toList votesFromOthers
    when (totalVotes > needed) $ replicateM_ (needed - forSelf) $ dispenseLaurel gsILaurelReserve gsLaurelsToDispense (:)
    gsInProgressVote .= Nothing

    --Figure out bribes
    bribes <- use gsBribes
    mapM_ transferBribe $ M.toList bribes
    gsBribes .= mempty

    --Is it the next players turn?
    haveCaeser <- use gsPickedUpCaeser
    anyLaurelsToGive <- not . null <$> use gsLaurelsToDispense
    unless (haveCaeser || anyLaurelsToGive) goToNextTurn

  DispenseSupportLaurel p -> do
    dispenseLaurel gsLaurelsToDispense (gsPlayerStates . at p . _Just . psLaurels) (:)
    gsVotes %= M.alter ((\vs -> if vs <= 1 then Nothing else Just (vs - 1)) =<<) p
    noMoreToGive <- null <$> use gsLaurelsToDispense
    when noMoreToGive goToNextTurn

  where
    goToNextTurn = do
      gsVotes .= mempty
      checkGameOver
      nextTurn

    checkGameOver = do
      Just i <- preuse $ innerSanctum . cPieces
      when (M.size i == 5) $ do
        beatRivals <- filterM hasBeatenRival $ M.keys i
        gsWinners .= Just beatRivals

    pointsAndOldest p = (,) <$> points p <*> oldestSenator 13 p
    hasBeatenRival p =  do
      (tP, oP) <- pointsAndOldest p
      (rivalP, rivalO) <- preuse (gsRivals . at p . _Just) >>= \case
        Nothing -> pure (0, maxBound)
        Just r -> pointsAndOldest r
      if tP == rivalP
      then pure $ oP < rivalO
      else pure $ tP > rivalP

    laurelsForPlayer p = gsPlayerStates . at p . _Just . psLaurels

    transferBribe (p,b) = do
      currentTurn <- use gsCurrentTurn
      laurelsForPlayer currentTurn %= \ls -> (L.\\) ls b
      laurelsForPlayer p %= (++) b

    giveSupportLaurel (p,votes) = replicateM_ votes $ dispenseLaurel gsILaurelReserve (laurelsForPlayer p) (:)

    moveSenator e = do
      currentTurn <- use gsCurrentTurn
      Just mL <- preuse $ edge e
      caeser <- (==) e <$> use gsCaeser
      unless caeser $ do
        gsPlayerStates . at currentTurn . _Just . psLaurels %= (++) (fromMaybe [] $ pure <$> mL)
        gsPickedUpCaeser .= maybe False (view lIsCaeser) mL
        dispenseLaurel gsLaurelReserve (edge e) $ const . Just
      removeFromCommitte $ e ^. eFrom
      addToCommittee $  e ^. eTo

    removeFromCommitte = changeCommittee (const $ Just . maybe [] (fromMaybe [] . preview _init))
    addToCommittee = changeCommittee (\m -> Just . maybe [m+1] (++ [m+1]))
    changeCommittee f s = do
      currentTurn <- use gsCurrentTurn
      (boardSpace s . cPieces) %= \pcs ->
        let
          maxInC = maybe 0 (fromMaybe 0 . maximumOf traverse) $ maximumOf traverse pcs
        in M.alter (f maxInC) currentTurn pcs

    numForSelf s = do
      currentTurn <- use gsCurrentTurn
      fromMaybe (0 :: Int) <$> preuse (boardSpace s . cPieces . at currentTurn . _Just . to length)

    votesNeeded s = do
      maxInC <- fromMaybe 0 <$> preuse (boardSpace s . cNum)
      pure $ ceiling $ fromIntegral maxInC / 2

    nextTurn = do
      numPlayers <- M.size <$> use gsPlayerStates
      gsCurrentTurn %= \t -> if t == numPlayers then 1 else t + 1

    caeserEdge e = (==) e <$> use gsCaeser

makeMoves :: GameState -> [Move] -> GameState
makeMoves = foldl' makeMove

laurelEdges :: [Edge]
laurelEdges = [Edge 8 12, Edge 12 13, Edge 1 8, Edge 1 5, Edge 9 13, Edge 6 9, Edge 6 11, Edge 14 13, Edge 3 6, Edge 3 7]

moves :: Player -> GameState -> [Move]
moves p gs
  | isJust (gs ^. gsWinners) = []
  | isJust (gs ^. gsInProgressVote) = voteInProgressMoves
  | gs ^. gsPickedUpCaeser = caeserMoves
  | not (null (gs ^. gsLaurelsToDispense)) = map DispenseSupportLaurel $ M.keys $ gs ^. gsVotes
  | myTurn = caeserMoves ++ startSenators ++ callVotes
  | otherwise = []
  where
    voteInProgressMoves = if myTurn
      then [Bribe p2 ls | p2 <- M.keys votingPeople, ls <- S.toList (S.fromList (L.subsequences laurelsToGive)), not (null ls) ]
      else [CastVote p v | v <- maybe [] (enumFromTo 1) (length <$> M.lookup p votingPeople)]
    caeserMoves = map MoveCaeser laurelEdges
    startSenators = if fromMaybe False (gs ^? gsPlayerStates . at (gs ^. gsCurrentTurn) . _Just . psReserve . to (> 0))
      then map StartSenator openStartingSpots
      else []
    callVotes = concatMap (map CallVote . adjacentTo gs) (gs ^.. spacesWithSenatorFor p . _1)
    openStartingSpots = filter (spotsAvailable (gs ^. gsBoard)) [0..3]
    myTurn = gs ^. gsCurrentTurn == p
    laurelsToGive = fromMaybe [] $ gs ^? gsPlayerStates . at (gs ^. gsCurrentTurn) . _Just . psLaurels
    votingPeople = maybe mempty (M.filterWithKey (\p2 _ -> (gs ^. gsCurrentTurn) /= p2) . view cPieces) committeVoting
    committeVoting = do
      (Edge f _) <- gs ^. gsInProgressVote
      M.lookup f $ gs ^. gsBoard
