{-|
Module      : Formula.Glycerolipid
Description : Instances of MolecularFormula for the glycerolipid data types provided.
Copyright   : Michael Thomas
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental
-}

module Formula.Glycerolipid where

import Lipid.Glycerolipid
import Formula.Blocks

instance MolecularFormulae MG where
    getFormula lipid = case lipid of
                            (ClassLevelMG _) -> Nothing
                            (UnknownSn x)    -> lookupBlockComp "glycerolBackbone"
                                             |+| 2 |*| lookupBlockComp "hydroxyl"
                                             |+| getFormula x
                            (Sn1MG x)        -> lookupBlockComp "glycerolBackbone"
                                             |+| 2 |*| lookupBlockComp "hydroxyl"
                                             |+| getFormula x
                            (Sn2MG x)        -> lookupBlockComp "glycerolBackbone"
                                             |+| 2 |*| lookupBlockComp "hydroxyl"
                                             |+| getFormula x
                            (Sn3MG x)        -> lookupBlockComp "glycerolBackbone"
                                             |+| 2 |*| lookupBlockComp "hydroxyl"
                                             |+| getFormula x

instance MolecularFormulae DG where
    getFormula lipid = case lipid of
                            (ClassLevelDG _)     -> Nothing
                            (CombinedRadylsDG x) -> lookupBlockComp "glycerolBackbone"
                                                 |+| lookupBlockComp "hydroxyl"
                                                 |+| getFormula x
                            (UnknownDG (x, y))   -> lookupBlockComp "glycerolBackbone"
                                                 |+| lookupBlockComp "hydroxyl"
                                                 |+| getFormula x
                                                 |+| getFormula y
                            (Sn12DG x y)         -> lookupBlockComp "glycerolBackbone"
                                                 |+| lookupBlockComp "hydroxyl"
                                                 |+| getFormula x
                                                 |+| getFormula y
                            (Sn13DG x y)         -> lookupBlockComp "glycerolBackbone"
                                                 |+| lookupBlockComp "hydroxyl"
                                                 |+| getFormula x
                                                 |+| getFormula y
                            (Sn23DG x y)         -> lookupBlockComp "glycerolBackbone"
                                                 |+| lookupBlockComp "hydroxyl"
                                                 |+| getFormula x
                                                 |+| getFormula y

instance MolecularFormulae TG where
    getFormula lipid = case lipid of
                            (ClassLevelTG _)        -> Nothing
                            (CombinedRadylsTG x)    -> lookupBlockComp "glycerolBackbone"
                                                    |+| getFormula x
                            (UnknownSnTG (x, y, z)) -> lookupBlockComp "glycerolBackbone"
                                                    |+| getFormula x
                                                    |+| getFormula y
                                                    |+| getFormula z
                            (KnownSnTG x y z)       -> lookupBlockComp "glycerolBackbone"
                                                    |+| getFormula x
                                                    |+| getFormula y
                                                    |+| getFormula z

