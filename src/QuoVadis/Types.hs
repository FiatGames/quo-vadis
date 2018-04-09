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
  , _gsBribes            :: !(HashMap Player (HashMap Player [Laurel]))
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
  | Bribe Player Player [Laurel]
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

laurelsForPlayer :: Player -> Traversal' GameState [Laurel]
laurelsForPlayer p = gsPlayerStates . at p . _Just . psLaurels

senatorsInReserve :: Player -> Traversal' GameState Int
senatorsInReserve p = gsPlayerStates . at p . _Just . psReserve

senatorsAtSpot :: Player -> BoardSpace -> Traversal' GameState [PlaceInCommittee]
senatorsAtSpot p s = gsBoard . at s . _Just . cPieces . at p . _Just

--- Folds ---
spacesWithSenatorFor :: Player -> Fold GameState (BoardSpace, Committee)
spacesWithSenatorFor p = gsBoard . folded . withIndex . filtered (anyOf (_2 . cPieces . at p . _Just . to length) (> 0))

totalPoints :: Fold GameState (Player, Int)
totalPoints = gsPlayerStates . folded . withIndex . alongside id (to (sumOf (psLaurels . traverse . lScore)))

pointsForPlayer :: Player -> Fold GameState Int
pointsForPlayer p = totalPoints . filtered ((==) p . view _1) . _1

oldestSenator :: BoardSpace -> Player -> Fold GameState Int
oldestSenator s p = boardSpace s . cPieces . at p . _Just . to minimum

votesNeeded :: BoardSpace -> Fold GameState Int
votesNeeded s = boardSpace s . cNum . to (\m -> ceiling $ fromIntegral m / 2)

--- Helpers ---
spotsAvailable :: GameState -> BoardSpace -> Bool
spotsAvailable gs x = case gs ^. gsBoard . at x of
    Nothing -> False
    Just c  -> (c^.cNum) > sumOf (cPieces . traversed . to length) c

adjacentTo :: GameState -> BoardSpace -> [Edge]
adjacentTo gs s = filter (\(Edge f t) -> s == f && spotsAvailable gs t) $ M.keys $ gs ^. gsEdges

hasBeatenRival :: GameState -> Player -> Bool
hasBeatenRival gs p
  | myPoints == theirPoints = myOldest > theirOldest
  | otherwise = myPoints > theirPoints
  where
    myPoints = fromMaybe 0 $ gs ^? pointsForPlayer p
    myOldest = fromMaybe maxBound $ gs ^? oldestSenator 13 p
    myRival = fromMaybe p $ gs ^? gsRivals . at p . _Just
    theirPoints = fromMaybe 0 $ gs ^? pointsForPlayer myRival
    theirOldest = fromMaybe maxBound $ gs ^? oldestSenator 13 myRival

makeMove :: GameState -> Move -> GameState
makeMove gs mv = flip execState gs $ case mv of
  MoveCaeser e -> do
    gsCaeser .= e
    gsPickedUpCaeser .= False
    goToNextTurn

  StartSenator s -> do
    addToCommittee s
    currPlayer <- use gsCurrentTurn
    senatorsInReserve currPlayer -= 1
    goToNextTurn

  CallVote e -> do
      forSelf <- numForSelf $ e ^. eFrom
      needed <- fromMaybe 0 <$> preuse (votesNeeded (e ^. eFrom))
      caeser <- (==) e <$> use gsCaeser
      if forSelf >= needed || caeser
      then do
        moveSenator e
        haveCaeser <- use gsPickedUpCaeser
        unless haveCaeser nextTurn
      else gsInProgressVote .= Just e

  CastVote p votes -> gsVotes %= M.alter (const $ if votes == 0 then Nothing else Just votes) p

  Bribe p1 p2 ls -> gsBribes . at p1 %= Just . \case
    Nothing -> M.singleton p2 ls
    Just m -> M.alter (const $ if null ls then Nothing else Just ls) p2 m

  VoteOver -> do
    --Figure out votes
    Just e <- use gsInProgressVote
    forSelf <- numForSelf $ e^. eFrom
    votesFromOthers <- use gsVotes
    let numVotesFromOthers = sum votesFromOthers
        totalVotes = forSelf + numVotesFromOthers
    needed <- fromMaybe 0 <$> preuse (votesNeeded (e^. eFrom))
    when (totalVotes >= needed) $ moveSenator e
    when (totalVotes == needed) $ mapM_ giveSupportLaurel $ M.toList votesFromOthers
    when (totalVotes > needed) $ do
      anyInReserve <- not . null <$> use gsILaurelReserve
      when anyInReserve $ replicateM_ (needed - forSelf) $ dispenseLaurelToList gsILaurelReserve gsLaurelsToDispense
    gsInProgressVote .= Nothing

    --Figure out bribes
    bribes <- use gsBribes
    mapM_ (\(p,bs) -> mapM_ (uncurry (transferBribe p)) bs) $ M.toList $ fmap M.toList bribes
    gsBribes .= mempty

    --Is it the next players turn?
    haveCaeser <- use gsPickedUpCaeser
    anyLaurelsToGive <- not . null <$> use gsLaurelsToDispense
    unless (haveCaeser || anyLaurelsToGive) goToNextTurn

  DispenseSupportLaurel p -> do
    dispenseLaurelToList gsLaurelsToDispense (laurelsForPlayer p)
    gsVotes %= M.alter ((\vs -> if vs <= 1 then Nothing else Just (vs - 1)) =<<) p

    emptyReserve <- null <$> use gsILaurelReserve
    when emptyReserve $ gsLaurelsToDispense .= mempty

    noMoreToGive <- null <$> use gsLaurelsToDispense
    when noMoreToGive goToNextTurn

  where
    goToNextTurn = do
      gsVotes .= mempty
      w <- setGameOver
      when (isNothing w) nextTurn

    setGameOver = do
      Just i <- preuse $ innerSanctum . cPieces
      when (M.size i == 5) $ do
        beaten <- hasBeatenRival <$> get
        gsWinners .= Just (filter beaten $ M.keys i)
      use gsWinners

    transferBribe p1 p2 b = do
      laurelsForPlayer p1 %= \ls -> (L.\\) ls b
      laurelsForPlayer p2 %= (++) b

    dispenseLaurel :: MonadState GameState m => (Laurel -> f Laurel -> f Laurel) -> Lens' GameState [Laurel] -> ASetter' GameState (f Laurel) -> m ()
    dispenseLaurel with f t = do
      ls <- use f
      unless (null ls) $ do
        f .= tail ls
        t %= with (head ls)

    dispenseLaurelToList :: MonadState GameState m => Lens' GameState [Laurel] -> ASetter' GameState [Laurel] -> m ()
    dispenseLaurelToList = dispenseLaurel (:)

    giveSupportLaurel (p,votes) = replicateM_ votes $ dispenseLaurelToList gsILaurelReserve (laurelsForPlayer p)

    moveSenator e = do
      currPlayer <- use gsCurrentTurn
      Just mL <- preuse $ edge e
      caeser <- (==) e <$> use gsCaeser
      unless caeser $ do
        laurelsForPlayer currPlayer %= (++) (fromMaybe [] $ pure <$> mL)
        gsPickedUpCaeser .= maybe False (view lIsCaeser) mL
        dispenseLaurel (const . Just) gsLaurelReserve (edge e)
      removeFromCommitte $ e ^. eFrom
      addToCommittee $  e ^. eTo

    removeFromCommitte = changeCommittee (const $ Just . maybe [] (fromMaybe [] . preview _init))
    addToCommittee = changeCommittee (\m -> Just . maybe [m+1] (++ [m+1]))
    changeCommittee f s = do
      currPlayer <- use gsCurrentTurn
      (boardSpace s . cPieces) %= \pcs ->
        let maxInC = fromMaybe 0 $ maximumOf (traverse . traverse) pcs
        in M.alter (f maxInC) currPlayer pcs

    numForSelf s = do
      currPlayer <- use gsCurrentTurn
      fromMaybe 0 <$> preuse (senatorsAtSpot currPlayer s . to length)

    nextTurn = do
      numPlayers <- M.size <$> use gsPlayerStates
      gsCurrentTurn %= \t -> if t == numPlayers then 1 else t + 1

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
    myTurn = gs ^. gsCurrentTurn == p
    voteInProgressMoves = [CastVote p v | v <- maybe [] (enumFromTo 1) (length <$> M.lookup p votingPeople)]
    caeserMoves = map MoveCaeser laurelEdges
    startSenators = if fromMaybe False (gs ^? senatorsInReserve (gs ^. gsCurrentTurn) . to (> 0))
      then map StartSenator openStartingSpots
      else []
    callVotes = concatMap (map CallVote . adjacentTo gs) (gs ^.. spacesWithSenatorFor p . _1)
    openStartingSpots = filter (spotsAvailable gs) [0..3]
    votingPeople = maybe mempty (M.filterWithKey (\p2 _ -> (gs ^. gsCurrentTurn) /= p2) . view cPieces) committeVoting
    committeVoting = do
      (Edge f _) <- gs ^. gsInProgressVote
      M.lookup f $ gs ^. gsBoard
