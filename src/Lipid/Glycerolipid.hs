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
import Control.Lens
import Data.Monoid ((<>))


data MG a
  = ClassLevelMG Integer
  | UnknownSn    (Radyl a)
  | Sn1MG        (Glycerol (Radyl a) () ())
  | Sn2MG        (Glycerol () (Radyl a) ())
  | Sn3MG        (Glycerol () () (Radyl a))
  deriving (Show, Eq, Ord)

makePrisms ''MG

data DG a
  = ClassLevelDG     Integer
  | CombinedRadylsDG (TwoCombinedRadyls a)
  | UnknownDG        (Radyl a) (Radyl a)
  | Sn12DG           (Glycerol (Radyl a) (Radyl a) ())
  | Sn13DG           (Glycerol (Radyl a) () (Radyl a))
  | Sn23DG           (Glycerol () (Radyl a) (Radyl a))
  deriving (Show, Eq, Ord)

makePrisms ''DG

data TG a
  = ClassLevelTG     Integer
  | CombinedRadylsTG (ThreeCombinedRadyls a)
  | UnknownSnTG      (Radyl a) (Radyl a) (Radyl a)
  | KnownSnTG        (Glycerol (Radyl a) (Radyl a) (Radyl a))
  deriving (Show, Eq, Ord)

makePrisms ''TG

instance Shorthand a => Shorthand (MG a) where
    shorthand l =
      case l of
        ClassLevelMG n -> "MG (" <> show n <> ")"
        UnknownSn r    -> "MG " <> shorthand r
        Sn1MG g        -> "MG " <> shorthand g
        Sn2MG g        -> "MG " <> shorthand g
        Sn3MG g        -> "MG " <> shorthand g

instance NNomenclature a => NNomenclature (MG a) where
    nNomenclature l =
      case l of
        ClassLevelMG n -> "MG (" <> show n <> ")"
        UnknownSn r    -> "MG " <> nNomenclature r
        Sn1MG g        -> "MG " <> nNomenclature g <> "/0:0/0:0"
        Sn2MG g        -> "MG 0:0/" <> nNomenclature g <> "/0:0"
        Sn3MG g        -> "MG 0:0/0:0/" <> nNomenclature g

instance Shorthand a => Shorthand (DG a) where
    shorthand l =
      case l of
        ClassLevelDG n      -> "DG (" <> show n <> ")"
        CombinedRadylsDG gs -> "DG " <> shorthand gs
        UnknownDG g1 g2     -> "DG " <> shorthand g1 <> "_" <> shorthand g2
        Sn12DG g            -> "DG " <> shorthand g
        Sn13DG g            -> "DG " <> shorthand g
        Sn23DG g            -> "DG 0:0" <> shorthand g

instance NNomenclature a => NNomenclature (DG a) where
    nNomenclature l =
      case l of
        ClassLevelDG n      -> "DG (" <> show n <> ")"
        CombinedRadylsDG gs -> "DG " <> nNomenclature gs
        UnknownDG g1 g2     -> "DG " <> nNomenclature g1 <> "_" <> nNomenclature g2
        Sn12DG g            -> "DG " <> nNomenclature g
        Sn13DG g            -> "DG " <> nNomenclature g
        Sn23DG g            -> "DG 0:0" <> nNomenclature g

instance Shorthand a => Shorthand (TG a) where
  shorthand l =
    case l of
      ClassLevelTG n       -> "TG (" <> show n <> ")"
      CombinedRadylsTG gs  -> "TG " <> shorthand gs
      UnknownSnTG g1 g2 g3 -> renderTG shorthand "_" g1 g2 g3
      KnownSnTG g          -> shorthand g

instance NNomenclature a => NNomenclature (TG a) where
  nNomenclature l =
    case l of
      ClassLevelTG n       -> "TG (" <> show n <> ")"
      CombinedRadylsTG gs  -> "TG " <> nNomenclature gs
      UnknownSnTG g1 g2 g3 -> renderTG nNomenclature "_" g1 g2 g3
      KnownSnTG g          -> nNomenclature g

renderTG f sep g1 g2 g3  = "TG " <> g1' <> sep <> g2' <> sep <> g3'
    where g1' = f g1
          g2' = f g2
          g3' = f g3
