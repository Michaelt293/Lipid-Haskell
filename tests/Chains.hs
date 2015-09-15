{-|
Module      : Chains
Description :
Copyright   : Michael Thomas
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : experimental
-}

module Chains where

import BuildingBlocks
import LipidClasses

c18_3 = SimpleCarbonChain (Carbons 18) [ DoubleBond (Just (Delta 9)) (Just Cis)
                                       , DoubleBond (Just (Delta 12)) (Just Cis)
                                       , DoubleBond (Just (Delta 15)) (Just Cis)]

c18_1 = SimpleCarbonChain (Carbons 18) [DoubleBond (Just (Delta 9)) (Just Cis)]

c16_0 = SimpleCarbonChain (Carbons 16) []


ala = FA c18_3

oleicAcid = FA c18_1

palmiticAcid = FA c16_0 


