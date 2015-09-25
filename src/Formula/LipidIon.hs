{-|
Module      : Formula.LipidIon
Description : Instance of MolecularFormula for the LipidIon data type provided. 
Copyright   : Michael Thomas
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental
-}

module Formula.LipidIon where

import qualified Data.Map.Strict as Map
import Lipid.Blocks
import Ion.LipidIon
import Formula.Blocks


instance MolecularFormulae LipidIon where
    getFormula lipidIon =
        case lipidIon of
             (FAion    l p as) -> getLipidFormula l p as
             (MGion    l p as) -> getLipidFormula l p as
             (DGion    l p as) -> getLipidFormula l p as
             (TGion    l p as) -> getLipidFormula l p as
             (PAion    l p as) -> getLipidFormula l p as
             (PCion    l p as) -> getLipidFormula l p as
             (PEion    l p as) -> getLipidFormula l p as
             (PGion    l p as) -> getLipidFormula l p as
             (PGPion   l p as) -> getLipidFormula l p as
             (PIion    l p as) -> getLipidFormula l p as
             (PIPion   l p as) -> getLipidFormula l p as
             (PIP2ion  l p as) -> getLipidFormula l p as
             (PIP3ion  l p as) -> getLipidFormula l p as
             (PSion    l p as) -> getLipidFormula l p as
             (CLion    l p as) -> getLipidFormula l p as
             (BMPion   l p as) -> getLipidFormula l p as
             (LPAion   l p as) -> getLipidFormula l p as
             (LPCion   l p as) -> getLipidFormula l p as
             (LPEion   l p as) -> getLipidFormula l p as
             (LPGion   l p as) -> getLipidFormula l p as
             (LPGPion  l p as) -> getLipidFormula l p as
             (LPIion   l p as) -> getLipidFormula l p as
             (LPIPion  l p as) -> getLipidFormula l p as
             (LPIP2ion l p as) -> getLipidFormula l p as
             (LPIP3ion l p as) -> getLipidFormula l p as
             (LPSion   l p as) -> getLipidFormula l p as    
             (LCLion   l p as) -> getLipidFormula l p as


getLipidFormula l p as = getFormula l
                      |+| p |*| lookupAdductComp H'
                      |+| (foldr (|+|) (Just (MolecularFormula (Map.fromList []))) 
                           $ map (\ (x, y) -> y |*| (lookupAdductComp x)) as)





