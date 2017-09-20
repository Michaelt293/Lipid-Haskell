{-|
Module      : Lipid.UnknownSn.Glycerolipid.hs
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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE LambdaCase #-}


module Lipid.UnknownSn.Glycerolipid where

import Lipid.Blocks
import Lipid.Format
import Control.Lens
import Data.Monoid ((<>))
import Data.List (sort)

newtype MG a = MG
  { radyl :: Radyl a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeClassy ''MG

data DG a = DG
  { radyl1DG :: Radyl a
  , radyl2DG :: Radyl a
  } deriving (Show, Functor, Foldable, Traversable) -- add Ord

makeClassy ''DG

instance (Eq a, Ord a) => Eq (DG a) where
  DG r1 r2 == DG r1' r2' =
    sort [r1, r2] == sort [r1', r2']

instance Ord a => Ord (DG a) where
  DG r1 r2 `compare` DG r1' r2' =
    sort [r1, r2] `compare` sort [r1', r2']

data TG a = TG
  { radyl1TG :: Radyl a
  , radyl2TG :: Radyl a
  , radyl3TG :: Radyl a
  } deriving (Show, Functor, Foldable, Traversable)

makeClassy ''TG

instance (Eq a, Ord a) => Eq (TG a) where
  TG r1 r2 r3 == TG r1' r2' r3' =
    sort [r1, r2, r3] == sort [r1', r2', r3']

instance Ord a => Ord (TG a) where
  TG r1 r2 r3 `compare` TG r1' r2' r3' =
    sort [r1, r2, r3] `compare` sort [r1', r2', r3']

instance Shorthand a => Shorthand (MG a) where
  shorthand (MG r) = "MG " <> shorthand r

instance NNomenclature a => NNomenclature (MG a) where
    nNomenclature (MG r) = "MG " <> nNomenclature r

instance Shorthand a => Shorthand (DG a) where
    shorthand (DG r1 r2) = "DG " <> shorthand r1 <> "_" <> shorthand r2

instance NNomenclature a => NNomenclature (DG a) where
    nNomenclature (DG r1 r2) = "DG " <> nNomenclature r1 <> "_" <> nNomenclature r2

instance Shorthand a => Shorthand (TG a) where
  shorthand (TG r1 r2 r3) = renderTG shorthand "_" r1 r2 r3

instance NNomenclature a => NNomenclature (TG a) where
  nNomenclature (TG r1 r2 r3) = renderTG nNomenclature "_" r1 r2 r3

renderTG f sep g1 g2 g3  = "TG " <> g1' <> sep <> g2' <> sep <> g3'
    where g1' = f g1
          g2' = f g2
          g3' = f g3
