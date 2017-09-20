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
  | Sn1MG        (KnownSn.Glycerolipid.MG1 a)
  | Sn2MG        (KnownSn.Glycerolipid.MG2 a)
  | Sn3MG        (KnownSn.Glycerolipid.MG3 a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makePrisms ''MG

data DG a
  = ClassLevelDG     ClassLevel.Glycerolipid.DG
  | CombinedRadylsDG (CombinedRadyl.Glycerolipid.DG a)
  | UnknownSnDG      (UnknownSn.Glycerolipid.DG a)
  | Sn12DG           (KnownSn.Glycerolipid.DG12 a)
  | Sn13DG           (KnownSn.Glycerolipid.DG13 a)
  | Sn23DG           (KnownSn.Glycerolipid.DG23 a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makePrisms ''DG

data TG a
  = ClassLevelTG     ClassLevel.Glycerolipid.DG
  | CombinedRadylsTG (CombinedRadyl.Glycerolipid.TG a)
  | UnknownSnTG      (UnknownSn.Glycerolipid.TG a)
  | KnownSnTG        (KnownSn.Glycerolipid.TG a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makePrisms ''TG

instance Shorthand a => Shorthand (MG a) where
    shorthand l =
      case l of
        ClassLevelMG c -> shorthand c
        UnknownSnMG r  -> shorthand r
        Sn1MG g        -> shorthand g
        Sn2MG g        -> shorthand g
        Sn3MG g        -> shorthand g

instance NNomenclature a => NNomenclature (MG a) where
    nNomenclature l =
      case l of
        ClassLevelMG c -> shorthand c
        UnknownSnMG g  -> nNomenclature g
        Sn1MG g        -> nNomenclature g
        Sn2MG g        -> nNomenclature g
        Sn3MG g        -> nNomenclature g

instance Shorthand a => Shorthand (DG a) where
    shorthand l =
      case l of
        ClassLevelDG c      -> shorthand c
        CombinedRadylsDG rs -> shorthand rs
        UnknownSnDG g       -> shorthand g
        Sn12DG g            -> shorthand g
        Sn13DG g            -> shorthand g
        Sn23DG g            -> shorthand g

instance NNomenclature a => NNomenclature (DG a) where
    nNomenclature l =
      case l of
        ClassLevelDG c      -> shorthand c
        CombinedRadylsDG rs -> nNomenclature rs
        UnknownSnDG g       -> nNomenclature g
        Sn12DG g            -> nNomenclature g
        Sn13DG g            -> nNomenclature g
        Sn23DG g            -> nNomenclature g

instance Shorthand a => Shorthand (TG a) where
  shorthand l =
    case l of
      ClassLevelTG c       -> shorthand c
      CombinedRadylsTG rs  -> shorthand rs
      UnknownSnTG g        -> shorthand g
      KnownSnTG g          -> shorthand g

instance NNomenclature a => NNomenclature (TG a) where
  nNomenclature l =
    case l of
      ClassLevelTG c       -> shorthand c
      CombinedRadylsTG rs  -> nNomenclature rs
      UnknownSnTG g        -> nNomenclature g
      KnownSnTG g          -> nNomenclature g
