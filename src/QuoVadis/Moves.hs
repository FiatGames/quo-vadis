{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}

module QuoVadis.Moves where

import           Control.Lens
import           Control.Monad
import           Control.Monad.State.Strict
import qualified Data.HashMap.Strict        as M
import           Data.List                  as L
import           Data.Maybe
import qualified Data.Set                   as S
import           GHC.Generics               hiding (from, to)
import           QuoVadis.Types

makeMoves :: GameState -> [Move] -> GameState
makeMoves = foldl' makeMove

data Move
  = MoveCaeser Edge
  | StartSenator BoardSpace
  | CallVote Edge
  | CastVote Player NumVotes
  | Bribe Player Player [Laurel]
  | VoteOver
  | DispenseSupportLaurel Player
  | Pass
  deriving (Eq, Show, Ord, Generic)

makeMove :: GameState -> Move -> GameState
makeMove gs mv = flip execState gs $ case mv of
  MoveCaeser e -> do
    gsCaeser .= e
    gsPickedUpACaeserLaurel .= False
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
        haveCaeser <- use gsPickedUpACaeserLaurel
        unless haveCaeser goToNextTurn
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
    haveCaeser <- use gsPickedUpACaeserLaurel
    anyLaurelsToGive <- not . null <$> use gsLaurelsToDispense
    unless (haveCaeser || anyLaurelsToGive) goToNextTurn

  DispenseSupportLaurel p -> do
    dispenseLaurelToList gsLaurelsToDispense (laurelsForPlayer p)
    gsVotes %= M.alter ((\vs -> if vs <= 1 then Nothing else Just (vs - 1)) =<<) p

    emptyReserve <- null <$> use gsILaurelReserve
    when emptyReserve $ gsLaurelsToDispense .= mempty

    noMoreToGive <- null <$> use gsLaurelsToDispense
    when noMoreToGive goToNextTurn

  Pass -> do
    ls <- use gsLaurelsToDispense
    unless (null ls) $ do
      validPlayers <- use gsVotes
      forM_ (M.toList validPlayers) $ \(p,vs) -> replicateM_ vs $ dispenseLaurelToList gsLaurelsToDispense (laurelsForPlayer p)
    goToNextTurn

  where
    goToNextTurn = do
      gsVotes .= mempty
      w <- setGameOver
      when (isNothing w) $ do
        numPlayers <- M.size <$> use gsPlayerStates
        gsCurrentTurn %= \t -> if t == numPlayers then 1 else t + 1

    setGameOver = do
      Just i <- preuse $ innerSanctum . cPieces
      when (sum (fmap length i) == 5) $ do
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
        gsPickedUpACaeserLaurel .= maybe False (view lIsCaeser) mL
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

bribes :: Player -> GameState -> [Move]
bribes p gs = [Bribe p p2 ls | p2 <- delete p $ M.keys (gs ^. gsPlayerStates) , ls <- S.toList (S.fromList (L.subsequences laurelsToGive)), not (null ls) ]
    where laurelsToGive = fromMaybe [] $ gs ^? laurelsForPlayer p

moves :: Player -> GameState -> [Move]
moves p gs
  | isJust (gs ^. gsWinners) = []
  | isJust (gs ^. gsInProgressVote) = VoteOver : voteInProgressMoves
  | gs ^. gsPickedUpACaeserLaurel = passIfMyTurn ++ caeserMoves
  | not (null (gs ^. gsLaurelsToDispense)) = passIfMyTurn ++ map DispenseSupportLaurel (M.keys $ gs ^. gsVotes)
  | myTurn = passIfMyTurn ++ caeserMoves ++ startSenators ++ callVotes
  | otherwise = []
  where
    passIfMyTurn = [Pass | myTurn]
    myTurn = gs ^. gsCurrentTurn == p
    voteInProgressMoves = [CastVote p v | v <- maybe [] (enumFromTo 1) (length <$> M.lookup p votingPeople)]
    caeserMoves = map MoveCaeser laurelEdges
    startSenators = if fromMaybe False (gs ^? senatorsInReserve (gs ^. gsCurrentTurn) . to (> 0))
      then map StartSenator openStartingSpots
      else []
    --Fix so that you can't call a vote in a committee that doesn't have enough senators, and also limit calling vote to a full committee
    callVotes = concatMap (map CallVote . adjacentTo gs) (gs ^.. spacesWithSenatorFor p . _1)
    openStartingSpots = filter (spotsAvailable gs) [0..3]
    votingPeople = maybe mempty (M.filterWithKey (\p2 _ -> (gs ^. gsCurrentTurn) /= p2) . view cPieces) committeVoting
    committeVoting = do
      (Edge f _) <- gs ^. gsInProgressVote
      M.lookup f $ gs ^. gsBoard
