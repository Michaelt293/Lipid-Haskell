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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Lipid.CombinedRadyl.Glycerolipid where

import Isotope
import Lipid.Blocks
import Control.Lens (makeLenses)
import Data.Monoid ((<>))

newtype TG a = TG
  { _getTG :: ThreeCombinedRadyls a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeLenses ''TG

instance Shorthand (ThreeCombinedRadyls a) => Shorthand (TG a) where
  shorthand (TG rs) = "TG " <> shorthand rs

instance NNomenclature (ThreeCombinedRadyls a) => NNomenclature (TG a) where
  nNomenclature (TG rs) ="TG " <> nNomenclature rs

instance HasThreeCombinedRadyls (TG a) a where
  threeCombinedRadyls = getTG

instance ToElementalComposition (TG a) where
  toElementalComposition (TG rs) =
    toElementalComposition rs
    <> mkElementalComposition [(C, 3), (H, 5)]
  charge _ = Just 0

newtype DG a = DG
  { _getDG :: TwoCombinedRadyls a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeLenses ''DG

instance Shorthand (TwoCombinedRadyls a) => Shorthand (DG a) where
  shorthand (DG rs) = "DG " <> shorthand rs

instance NNomenclature (TwoCombinedRadyls a) => NNomenclature (DG a) where
  nNomenclature (DG rs) = "DG " <> nNomenclature rs

instance HasTwoCombinedRadyls (DG a) a where
  twoCombinedRadyls = getDG

instance ToElementalComposition (DG a) where
  toElementalComposition (DG rs) =
    toElementalComposition rs
    <> mkElementalComposition [(C, 3), (H, 6), (O, 1)]
  charge _ = Just 0
