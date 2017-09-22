{-|
Module      : Lipid.CombinedRadyl.Glycerolipid
Description :
Copyright   : Michael Thomas
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Lipid.CombinedRadyl.Glycerolipid where

import Lipid.Blocks
import Control.Lens (makeLenses)
import Data.Monoid ((<>))
import Data.List (sort)

newtype TG a = TG
  { _getTG :: ThreeCombinedRadyls a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeLenses ''TG

instance Shorthand a => Shorthand (TG a) where
  shorthand (TG rs) = "DG " <> shorthand rs

instance NNomenclature a => NNomenclature (TG a) where
  nNomenclature (TG rs) ="DG " <> nNomenclature rs

instance HasThreeCombinedRadyls (TG a) a where
  threeCombinedRadyls = getTG

newtype DG a = DG
  { _getDG :: TwoCombinedRadyls a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeLenses ''DG

instance Shorthand a => Shorthand (DG a) where
  shorthand (DG rs) = "DG " <> shorthand rs

instance NNomenclature a => NNomenclature (DG a) where
  nNomenclature (DG rs) = "DG " <> nNomenclature rs

instance HasTwoCombinedRadyls (DG a) a where
  twoCombinedRadyls = getDG
