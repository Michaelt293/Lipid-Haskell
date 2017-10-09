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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Lipid.FattyAcid
    ( FA(..)
    ) where

import Lipid.Blocks
import Lipid.Format
import Data.Monoid ((<>))
import Control.Lens

data FA a
  = ClassLevelFA Integer
  | FA           (CarbonChain a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makePrisms ''FA

instance Shorthand (CarbonChain a) => Shorthand (FA a) where
    shorthand (ClassLevelFA x) = "FA " <> wrapParen (show x)
    shorthand (FA x)           = "FA " <> shorthand x

instance NNomenclature (CarbonChain a) => NNomenclature (FA a) where
    nNomenclature (ClassLevelFA x) = "FA " <> wrapParen (show x)
    nNomenclature (FA x)           = "FA " <> nNomenclature x

instance IsSaturated (FA a) where
  isSaturated (ClassLevelFA _) = Nothing
  isSaturated (FA cc) = isSaturated cc

instance IsMonounsaturated (FA a) where
  isMonounsaturated (ClassLevelFA cc) = Nothing
  isMonounsaturated (FA cc) = isMonounsaturated cc

instance IsPolyunsaturated (FA a) where
  isPolyunsaturated (ClassLevelFA cc) = Nothing
  isPolyunsaturated (FA cc) = isPolyunsaturated cc

instance Position a => IsBisAllylic (FA a) where
  isBisAllylic (ClassLevelFA cc) = Nothing
  isBisAllylic (FA cc) = isBisAllylic cc
