{-|
Module      : Lipid.Glycerolipid
Description : Glycerolipid data type and instances of Shorthand and
              NNomenclature defined.
Copyright   : Michael Thomas
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Lipid.Glycerolipid where

import Lipid.Blocks
import qualified Lipid.UnknownSn.Glycerolipid as UnknownSn.Glycerolipid
import qualified Lipid.KnownSn.Glycerolipid as KnownSn.Glycerolipid
import qualified Lipid.CombinedRadyl.Glycerolipid as CombinedRadyl.Glycerolipid
import qualified Lipid.ClassLevel.Glycerolipid as ClassLevel.Glycerolipid
import Control.Lens (makePrisms)
import Data.Monoid ((<>))
import Data.List (sort)


data MG a
  = ClassLevelMG ClassLevel.Glycerolipid.MG
  | UnknownSnMG  (UnknownSn.Glycerolipid.MG a)
  | KnownMG      (KnownSn.Glycerolipid.MG a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makePrisms ''MG

data DG a
  = ClassLevelDG     ClassLevel.Glycerolipid.DG
  | CombinedRadylsDG (CombinedRadyl.Glycerolipid.DG a)
  | UnknownSnDG      (UnknownSn.Glycerolipid.DG a)
  | KnownDG          (KnownSn.Glycerolipid.DG a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makePrisms ''DG

data TG a
  = ClassLevelTG     ClassLevel.Glycerolipid.DG
  | CombinedRadylsTG (CombinedRadyl.Glycerolipid.TG a)
  | UnknownSnTG      (UnknownSn.Glycerolipid.TG a)
  | KnownSnTG        (KnownSn.Glycerolipid.TG a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makePrisms ''TG

instance Shorthand (CarbonChain a) => Shorthand (MG a) where
  shorthand l =
    case l of
      ClassLevelMG c -> shorthand c
      UnknownSnMG r  -> shorthand r
      KnownMG g      -> shorthand g

instance NNomenclature (CarbonChain a) => NNomenclature (MG a) where
  nNomenclature l =
    case l of
      ClassLevelMG c -> shorthand c
      UnknownSnMG g  -> nNomenclature g
      KnownMG g      -> nNomenclature g

instance (Shorthand (CarbonChain a), Shorthand (TwoCombinedChains a)) => Shorthand (DG a) where
  shorthand l =
    case l of
      ClassLevelDG c      -> shorthand c
      CombinedRadylsDG rs -> shorthand rs
      UnknownSnDG g       -> shorthand g
      KnownDG g           -> shorthand g

instance (NNomenclature (CarbonChain a), NNomenclature (TwoCombinedChains a)) => NNomenclature (DG a) where
  nNomenclature l =
    case l of
      ClassLevelDG c      -> shorthand c
      CombinedRadylsDG rs -> nNomenclature rs
      UnknownSnDG g       -> nNomenclature g
      KnownDG g           -> nNomenclature g

instance (Shorthand (CarbonChain a), Shorthand (ThreeCombinedChains a)) => Shorthand (TG a) where
  shorthand l =
    case l of
      ClassLevelTG c       -> shorthand c
      CombinedRadylsTG rs  -> shorthand rs
      UnknownSnTG g        -> shorthand g
      KnownSnTG g          -> shorthand g

instance (NNomenclature (CarbonChain a), NNomenclature (ThreeCombinedChains a)) => NNomenclature (TG a) where
  nNomenclature l =
    case l of
      ClassLevelTG c       -> shorthand c
      CombinedRadylsTG rs  -> nNomenclature rs
      UnknownSnTG g        -> nNomenclature g
      KnownSnTG g          -> nNomenclature g
