{-|
Module      : Mass.Mass
Description : Functions for calculating masses from molecular formulae 
              and directly from molecules.
Copyright   : Michael Thomas
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental
-}

module Mass.Mass where

import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import ElementIsotopes hiding (monoisotopicMass, nominalMass)
import qualified ElementIsotopes as Elem (monoisotopicMass, nominalMass)
import Formula.Blocks
import Ion.LipidIon
import Formula.LipidIon


monoisotopicMassFormula :: MolecularFormula -> IsotopeMass
monoisotopicMassFormula (MolecularFormula comp) = sum [(Elem.monoisotopicMass elem) * (fromIntegral (find elem comp)) | 
                                                 elem <- Map.keys comp ]

averageMassFormula :: MolecularFormula -> IsotopeMass
averageMassFormula (MolecularFormula comp) = sum [(averageAtomicMass elem) * (fromIntegral (find elem comp)) | 
                                                 elem <- Map.keys comp ]

nominalMassFormula :: MolecularFormula -> IntegerMass
nominalMassFormula (MolecularFormula comp) = sum [(Elem.nominalMass elem) * (find elem comp) | 
                                                 elem <- Map.keys comp ]

monoisotopicMass :: MolecularFormulae a => a -> Maybe IsotopeMass
monoisotopicMass x  = fmap monoisotopicMassFormula $ getFormula x

averageMass :: MolecularFormulae a => a -> Maybe IsotopeMass
averageMass x = fmap averageMassFormula $ getFormula x

nominalMass :: MolecularFormulae a => a -> Maybe IntegerMass
nominalMass x = fmap nominalMassFormula $ getFormula x

mzRatio l = fmap (\ x -> x / (fromIntegral (lipidIonCharge l))) (monoisotopicMass l)
