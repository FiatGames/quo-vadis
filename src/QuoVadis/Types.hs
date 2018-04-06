{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module QuoVadis.Types where

import           Control.Lens
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           GHC.Generics

data Laurel = Laurel
  { _lScore    :: !Int
  , _lIsCaeser :: !Bool
  }
  deriving (Eq, Show, Generic)
makeLenses ''Laurel

type Player = Int

type Senator = ()

data Committee = Committee
  { _cNum    :: !Int
  , _cPieces :: !(Map Player [Senator])
  }
  deriving (Eq, Show, Generic)
makeLenses ''Committee

type BoardSpace = Int

type Board = Map BoardSpace Committee

type Edges = Map (BoardSpace,BoardSpace) (Maybe (Laurel, Bool))

data PlayerState = PlayerState
  { _psReserve :: ![Senator]
  , _psLaurels :: ![Laurel]
  }
  deriving (Eq, Show, Generic)
makeLenses ''PlayerState

data GameState = GameState
  { _gsPlayerStates   :: !(Map Player PlayerState)
  , _gsLaurelReserve  :: ![Laurel]
  , _gsBoard          :: !Board
  , _gsEdges          :: !Edges
  , _gsILaurelReserve :: ![Laurel]
  }
  deriving (Eq, Show, Generic)
makeLenses ''GameState

initialPlayerState :: PlayerState
initialPlayerState = PlayerState
  { _psReserve= replicate 8 ()
  , _psLaurels= []
  }

initialEdges :: [Laurel] -> ([Laurel], Edges)
initialEdges (l1:l2:l3:l4:l5:l6:l7:l8:l9:l10:ls) = (ls, M.fromList
  [ ((0,4),Nothing)
  , ((4,8),Nothing)
  , ((8,12),Just (l1,False))
  , ((12,13),Just (l2,False))
  , ((1,8),Just (l3,False))
  , ((1,5),Just (l4,False))
  , ((5,9),Nothing)
  , ((9,13),Just (l5,True))
  , ((2,6),Nothing)
  , ((6,9),Just (l6,False))
  , ((6,11),Just (l7,False))
  , ((11,14),Nothing)
  , ((14,13),Just (l8,False))
  , ((3,6),Just (l9,False))
  , ((3,7),Just (l10,False))
  , ((7,10),Nothing)
  , ((10,14),Nothing)
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
  }
  where (reserve', edges) = initialEdges reserve
