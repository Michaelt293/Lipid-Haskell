{-|
Module      : Smiles
Description :
Copyright   : Michael Thomas
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : experimental
-}

module Smiles where

import BuildingBlocks
import LipidClasses

c18_3 = SimpleCarbonChain 18 [ DoubleBond (Just (Delta 9)) (Just Cis)
                             , DoubleBond (Just (Delta 12)) (Just Cis)
                             , DoubleBond (Just (Delta 15)) (Just Cis)]

Sort double bonds


