{-|
Module      : Chains
Description :
Copyright   : Michael Thomas
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : experimental
-}

module Tests.Chains where

import Lipid.Blocks
import Lipid.FattyAcid

chain18_3 = SimpleCarbonChain (Carbons 18) [ DoubleBond (Just (Delta 9))  (Just Cis)
                                           , DoubleBond (Just (Delta 12)) (Just Cis)
                                           , DoubleBond (Just (Delta 15)) (Just Cis) ]

chain18_1 = SimpleCarbonChain (Carbons 18) [ DoubleBond (Just (Delta 9))  (Just Cis) ]

chain16_0 = SimpleCarbonChain (Carbons 16) []


alkyl18_3 = Radyl Alkyl chain18_3

alkyl18_1 = Radyl Alkyl chain18_1

alkyl16_0 = Radyl Alkyl chain16_0


acyl18_3 = Radyl Acyl chain18_3

acyl18_1 = Radyl Acyl chain18_1

acyl16_0 = Radyl Acyl chain16_0


alkenyl18_3 = Radyl Alkenyl chain18_3

alkenyl18_1 = Radyl Alkenyl chain18_1

alkenyl16_0 = Radyl Alkenyl chain16_0


ala = FA chain18_3

oleicAcid = FA chain18_1

palmiticAcid = FA chain16_0 


