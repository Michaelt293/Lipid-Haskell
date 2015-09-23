{-|
Module      : Formula.FattyAcid
Description : Instance of MolecularFormula for the FA data type provided. 
Copyright   : Michael Thomas
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental
-}

module Formula.FattyAcid where

import Lipid.Blocks
import Lipid.FattyAcid
import Formula.Blocks


instance MolecularFormulae FA where
    getFormula (ClassLevelFA _) = Nothing
    getFormula (FA x)           =  lookupBlockComp "Proton"
                               |+| getFormula x
                               |+| getFormula Acyl



