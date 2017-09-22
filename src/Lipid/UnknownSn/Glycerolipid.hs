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

newtype DG a = DG
  { twoRadylsDG :: TwoRadyls a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeClassy ''DG

newtype TG a = TG
  { threeRadyls :: ThreeRadyls a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeClassy ''TG

instance Shorthand a => Shorthand (MG a) where
  shorthand (MG r) = "MG " <> shorthand r

instance NNomenclature a => NNomenclature (MG a) where
    nNomenclature (MG r) = "MG " <> nNomenclature r

instance Shorthand a => Shorthand (DG a) where
    shorthand (DG rs) = "DG " <> shorthand rs

instance NNomenclature a => NNomenclature (DG a) where
    nNomenclature (DG rs) = "DG " <> nNomenclature rs

instance Shorthand a => Shorthand (TG a) where
  shorthand (TG rs) = "TG " <> shorthand rs

instance NNomenclature a => NNomenclature (TG a) where
  nNomenclature (TG rs) = "TG " <> nNomenclature rs
