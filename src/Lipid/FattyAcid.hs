{-|
Module      : FattyAcid
Description : FA data type and instances of Shorthand and NNomenclature defined.
Copyright   : Michael Thomas
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : experimental
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Lipid.FattyAcid
    (
      FA(..)
    ) where

import Lipid.Blocks
import Lipid.Format
import Data.Monoid ((<>))
import Control.Lens

data FA a b
  = ClassLevelFA Integer
  | FA           (CarbonChain a b)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makePrisms ''FA

instance Shorthand b => Shorthand (FA a b) where
    shorthand (ClassLevelFA x) = "FA " <> wrapParen (show x)
    shorthand (FA x)           = "FA " <> shorthand x

instance NNomenclature b => NNomenclature (FA a b) where
    nNomenclature (ClassLevelFA x) = "FA " <> wrapParen (show x)
    nNomenclature (FA x)           = "FA " <> nNomenclature x
