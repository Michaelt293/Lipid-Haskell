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
import Lipid.Format
import qualified Lipid.UnknownSn.Glycerolipid as UnknownSn.Glycerolipid
import qualified Lipid.KnownSn.Glycerolipid as KnownSn.Glycerolipid
import Control.Lens
import Data.Monoid ((<>))
import Data.List (sort)


data MG a
  = ClassLevelMG Integer
  | UnknownSnMG  (UnknownSn.Glycerolipid.MG a)
  | Sn1MG        (KnownSn.Glycerolipid.MG1 a)
  | Sn2MG        (KnownSn.Glycerolipid.MG2 a)
  | Sn3MG        (KnownSn.Glycerolipid.MG3 a)
  deriving (Show, Eq, Ord, Functor)

makePrisms ''MG

data DG a
  = ClassLevelDG     Integer
  | CombinedRadylsDG (TwoCombinedRadyls a)
  | UnknownSnDG      (UnknownSn.Glycerolipid.DG a)
  | Sn12DG           (KnownSn.Glycerolipid.DG12 a)
  | Sn13DG           (KnownSn.Glycerolipid.DG13 a)
  | Sn23DG           (KnownSn.Glycerolipid.DG23 a)
  deriving (Show, Eq, Functor)

makePrisms ''DG

-- instance (Eq a, Ord a) => Eq (DG a) where
--   ClassLevelDG n1 == ClassLevelDG n2 = n1 == n2
--   CombinedRadylsDG rs1 == CombinedRadylsDG rs2 = rs1 == rs2
--   UnknownDG r1 r2 == UnknownDG r3 r4 = sort [r1, r2] == sort [r3, r4]
--   Sn12DG g1 == Sn12DG g2 = g1 == g2
--   Sn13DG g1 == Sn13DG g2 = g1 == g2
--   Sn23DG g1 == Sn23DG g2 = g1 == g2
--   _ == _ = False

data TG a
  = ClassLevelTG     Integer
  | CombinedRadylsTG (ThreeCombinedRadyls a)
  | UnknownSnTG      (UnknownSn.Glycerolipid.TG a)
  | KnownSnTG        (KnownSn.Glycerolipid.TG a)
  deriving (Show, Eq, Functor)

makePrisms ''TG

-- instance (Eq a, Ord a) => Eq (TG a) where
--   ClassLevelTG n1 == ClassLevelTG n2 = n1 == n2
--   CombinedRadylsTG rs1 == CombinedRadylsTG rs2 = rs1 == rs2
--   UnknownSnTG r1 r2 r3 == UnknownSnTG r4 r5 r6 =
--     sort [r1, r2, r3] == sort [r4, r5, r6]
--   KnownSnTG g1 == KnownSnTG g2 = g1 == g2
--   _ == _ = False

instance Shorthand a => Shorthand (MG a) where
    shorthand l =
      case l of
        ClassLevelMG n -> "MG (" <> show n <> ")"
        UnknownSnMG g  -> shorthand g
        Sn1MG g        -> shorthand g
        Sn2MG g        -> shorthand g
        Sn3MG g        -> shorthand g

instance NNomenclature a => NNomenclature (MG a) where
    nNomenclature l =
      case l of
        ClassLevelMG n -> "MG (" <> show n <> ")"
        UnknownSnMG r  -> nNomenclature r
        Sn1MG g        -> nNomenclature g
        Sn2MG g        -> nNomenclature g
        Sn3MG g        -> nNomenclature g

instance Shorthand a => Shorthand (DG a) where
    shorthand l =
      case l of
        ClassLevelDG n      -> "DG (" <> show n <> ")"
        CombinedRadylsDG gs -> "DG " <> shorthand gs
        UnknownSnDG g       -> shorthand g
        Sn12DG g            -> shorthand g
        Sn13DG g            -> shorthand g
        Sn23DG g            -> shorthand g

instance NNomenclature a => NNomenclature (DG a) where
    nNomenclature l =
      case l of
        ClassLevelDG n      -> "DG (" <> show n <> ")"
        CombinedRadylsDG gs -> "DG " <> nNomenclature gs
        UnknownSnDG g       -> nNomenclature g
        Sn12DG g            -> nNomenclature g
        Sn13DG g            -> nNomenclature g
        Sn23DG g            -> nNomenclature g

instance Shorthand a => Shorthand (TG a) where
  shorthand l =
    case l of
      ClassLevelTG n       -> "TG (" <> show n <> ")"
      CombinedRadylsTG gs  -> "TG " <> shorthand gs
      UnknownSnTG g        -> shorthand g
      KnownSnTG g          -> shorthand g

instance NNomenclature a => NNomenclature (TG a) where
  nNomenclature l =
    case l of
      ClassLevelTG n       -> "TG (" <> show n <> ")"
      CombinedRadylsTG gs  -> "TG " <> nNomenclature gs
      UnknownSnTG g        -> nNomenclature g
      KnownSnTG g          -> nNomenclature g
