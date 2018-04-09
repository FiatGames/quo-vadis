{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}

module QuoVadis.Moves where

import           Control.Lens
import           Control.Monad
import           Control.Monad.State.Strict
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as M
import           Data.List                  as L
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set                   as S
import           QuoVadis.Types


