{-|
Module      : Formulae
Description :
Copyright   : Michael Thomas
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : experimental
-}

module Formulae where

import ElementIsotopes hiding (monoisotopicMass, nominalMass)
import qualified ElementIsotopes as Elem (monoisotopicMass, nominalMass)
import BuildingBlocks
import LipidClasses

data MolecularFormula = MolecularFormula [(ElementSymbol, Integer)]
                      deriving (Show, Eq, Ord)

class MolecularFormulae a where
    getFormula :: a -> MolecularFormula

instance MolecularFormulae FA where
    getFormula (FA (SimpleCarbonChain c db)) =
         MolecularFormula [(C, c), (O, 2), (H, (c - 1) * 2 + 2 - 2 * fromIntegral (length db))]


monoisotopicMass (MolecularFormula comp) = sum [(Elem.monoisotopicMass elem) * (fromIntegral num) | (elem, num) <- comp]

averageMass (MolecularFormula comp) = sum [(averageAtomicMass elem) * (fromIntegral num) | (elem, num) <- comp]

nominalMass (MolecularFormula comp) = sum [(Elem.nominalMass elem) * num | (elem, num) <- comp]

    
