{-|
Module      : FattyAcid
Description : FA data type and instances of Shorthand and Nomenclature defined.
Copyright   : Michael Thomas 
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : experimental
-}

module FattyAcid
    (
      FA
    ) where

import ElementIsotopes
import BuildingBlocks


data FA   = ClassLevelFA       IntegerMass
          | FA                 CarbonChain
          deriving (Show, Eq, Ord)


instance Shorthand FA where
    showShorthand (ClassLevelFA x) = "FA (" ++ show x ++ ")"
    showShorthand (FA x) = "FA " ++ showShorthand x

instance Nomenclature FA where
    showNnomenclature (ClassLevelFA x) = "FA (" ++ show x ++ ")"
    showNnomenclature (FA x)           = "FA " ++ showNnomenclature x

