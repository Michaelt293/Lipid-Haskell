{-|
Module      : ESI.FattyAcid
Description : Positive and negative ion ESI.
Copyright   : Michael Thomas
License     : GPL-3
aintainer   : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental
-}

module ESI.FattyAcid where

import Lipid.Lipid
import Lipid.FattyAcid
import Ion.LipidIon

instance Lipid FA where
    posESI fa = Just (FAion fa 1 [])  
    negESI fa = Just (FAion fa (-1) [])
