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
  | Sn1MG        (Radyl a)
  | Sn2MG        (Radyl a)
  | Sn3MG        (Radyl a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makePrisms ''MG

data DG a
  = ClassLevelDG     Integer
  | CombinedRadylsDG (TwoCombinedRadyls a)
  | UnknownDG        (Radyl a) (Radyl a)
  | Sn12DG           (Radyl a) (Radyl a)
  | Sn13DG           (Radyl a) (Radyl a)
  | Sn23DG           (Radyl a) (Radyl a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makePrisms ''DG

data TG a
  = ClassLevelTG     Integer
  | CombinedRadylsTG (ThreeCombinedRadyls a)
  | UnknownSnTG      (Radyl a) (Radyl a) (Radyl a)
  | KnownSnTG        (Radyl a) (Radyl a) (Radyl a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makePrisms ''TG

instance Shorthand a => Shorthand (MG a) where
    shorthand l =
      case l of
        ClassLevelMG n -> "MG (" <> show n <> ")"
        UnknownSn r    -> "MG " <> shorthand r
        Sn1MG r        -> "MG " <> shorthand r <> "/0:0/0:0"
        Sn2MG r        -> "MG 0:0/" <> shorthand r <> "/0:0"
        Sn3MG r        -> "MG 0:0/0:0/" <> shorthand r

instance NNomenclature a => NNomenclature (MG a) where
    nNomenclature l =
      case l of
        ClassLevelMG n -> "MG (" <> show n <> ")"
        UnknownSn r    -> "MG " <> nNomenclature r
        Sn1MG r        -> "MG " <> nNomenclature r <> "/0:0/0:0"
        Sn2MG r        -> "MG 0:0/" <> nNomenclature r <> "/0:0"
        Sn3MG r        -> "MG 0:0/0:0/" <> nNomenclature r

instance Shorthand a => Shorthand (DG a) where
    shorthand l =
      case l of
        ClassLevelDG n      -> "DG (" <> show n <> ")"
        CombinedRadylsDG rs -> "DG " <> shorthand rs
        UnknownDG r1 r2     -> "DG " <> shorthand r1 <> "_" <> shorthand r2
        Sn12DG r1 r2        -> "DG " <> shorthand r1 <> "/" <> shorthand r2 <> "/" <> "0:0"
        Sn13DG r1 r2        -> "DG " <> shorthand r1 <> "/" <> "0:0" <> "/" <> shorthand r2
        Sn23DG r1 r2        -> "DG 0:0" <> "/" <> shorthand r1 <> "/" <> shorthand r2

instance NNomenclature a => NNomenclature (DG a) where
    nNomenclature l =
      case l of
        ClassLevelDG n      -> "DG (" <> show n <> ")"
        CombinedRadylsDG rs -> "DG " <> nNomenclature rs
        UnknownDG r1 r2     -> "DG " <> nNomenclature r1 <> "_" <> nNomenclature r2
        Sn12DG r1 r2        -> "DG " <> nNomenclature r1 <> "/" <> nNomenclature r2 <> "/" <> "0:0"
        Sn13DG r1 r2        -> "DG " <> nNomenclature r1 <> "/" <> "0:0" <> "/" <> nNomenclature r2
        Sn23DG r1 r2        -> "DG 0:0" <> "/" <> nNomenclature r1 <> "/" <> nNomenclature r2

instance Shorthand a => Shorthand (TG a) where
  shorthand l =
    case l of
      ClassLevelTG n       -> "TG (" <> show n <> ")"
      CombinedRadylsTG rs  -> "TG " <> shorthand rs
      UnknownSnTG r1 r2 r3 -> renderTG shorthand "_" r1 r2 r3
      KnownSnTG r1 r2 r3   -> renderTG shorthand "/" r1 r2 r3

instance NNomenclature a => NNomenclature (TG a) where
  nNomenclature l =
    case l of
      ClassLevelTG n       -> "TG (" <> show n <> ")"
      CombinedRadylsTG rs  -> "TG " <> nNomenclature rs
      UnknownSnTG r1 r2 r3 -> renderTG nNomenclature "_" r1 r2 r3
      KnownSnTG r1 r2 r3   -> renderTG nNomenclature "/" r1 r2 r3

renderTG f sep r1 r2 r3  = "TG " <> r1' <> sep <> r2' <> sep <> r3'
    where r1' = f r1
          r2' = f r2
          r3' = f r3
