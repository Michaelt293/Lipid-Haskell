{-|
Module      : Formula.Glycerophospholipid
Description : Instances of MolecularFormula for glycerophospholipid
              data types provided.
Copyright   : Michael Thomas 
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental
-}

module Formula.Glycerophospholipid where


import Lipid.Glycerophospholipid
import Formula.Blocks


instance MolecularFormulae PA where
    getFormula lipid = case lipid of
                            (ClassLevelPA _)     -> Nothing
                            (CombinedRadylsPA x) -> lookupBlockComp "glycerolBackbone"
                                                 |+| lookupBlockComp "phosphate"
                                                 |+| lookupBlockComp "proton"
                                                 |+| getFormula x
                            (UnknownSnPA (x, y))  -> lookupBlockComp "glycerolBackbone"
                                                 |+| lookupBlockComp "phosphate"
                                                 |+| lookupBlockComp "proton"
                                                 |+| getFormula x
                                                 |+| getFormula y
                            (KnownSnPA x y)      -> lookupBlockComp "glycerolBackbone"
                                                 |+| lookupBlockComp "phosphate"
                                                 |+| lookupBlockComp "proton"
                                                 |+| getFormula x
                                                 |+| getFormula y

instance MolecularFormulae PC where
    getFormula lipid = case lipid of
                            (ClassLevelPC _)     -> Nothing
                            (CombinedRadylsPC x) -> lookupBlockComp "glycerolBackbone"
                                                 |+| lookupBlockComp "phosphate"
                                                 |+| lookupBlockComp "choline"
                                                 |+| getFormula x
                            (UnknownSnPC (x, y))  -> lookupBlockComp "glycerolBackbone"
                                                 |+| lookupBlockComp "phosphate"
                                                 |+| lookupBlockComp "choline"
                                                 |+| getFormula x
                                                 |+| getFormula y
                            (KnownSnPC x y)      -> lookupBlockComp "glycerolBackbone"
                                                 |+| lookupBlockComp "phosphate"
                                                 |+| lookupBlockComp "choline"
                                                 |+| getFormula x
                                                 |+| getFormula y

instance MolecularFormulae PE where
    getFormula lipid = case lipid of
                            (ClassLevelPE _)     -> Nothing
                            (CombinedRadylsPE x) -> lookupBlockComp "glycerolBackbone"
                                                 |+| lookupBlockComp "phosphate"
                                                 |+| lookupBlockComp "ethanolamine"
                                                 |+| getFormula x
                            (UnknownSnPE (x, y))  -> lookupBlockComp "glycerolBackbone"
                                                 |+| lookupBlockComp "phosphate"
                                                 |+| lookupBlockComp "ethanolamine"
                                                 |+| getFormula x
                                                 |+| getFormula y
                            (KnownSnPE x y)      -> lookupBlockComp "glycerolBackbone"
                                                 |+| lookupBlockComp "phosphate"
                                                 |+| lookupBlockComp "ethanolamine"
                                                 |+| getFormula x
                                                 |+| getFormula y

instance MolecularFormulae PG where
    getFormula lipid = case lipid of
                            (ClassLevelPG _)     -> Nothing
                            (CombinedRadylsPG x) -> lookupBlockComp "glycerolBackbone"
                                                 |+| lookupBlockComp "phosphate"
                                                 |+| lookupBlockComp "glycerol"
                                                 |+| getFormula x
                            (UnknownSnPG (x, y))  -> lookupBlockComp "glycerolBackbone"
                                                 |+| lookupBlockComp "phosphate"
                                                 |+| lookupBlockComp "glycerol"
                                                 |+| getFormula x
                                                 |+| getFormula y
                            (KnownSnPG x y)      -> lookupBlockComp "glycerolBackbone"
                                                 |+| lookupBlockComp "phosphate"
                                                 |+| lookupBlockComp "glycerol"
                                                 |+| getFormula x
                                                 |+| getFormula y

instance MolecularFormulae PGP where
    getFormula lipid = case lipid of
                            (ClassLevelPGP _) -> Nothing
                            (CombinedRadylsPGP x) -> lookupBlockComp "glycerolBackbone"
                                                  |+| lookupBlockComp "phosphate"
                                                  |+| lookupBlockComp "glycerolphosphate"
                                                  |+| getFormula x
                            (UnknownSnPGP (x, y))  -> lookupBlockComp "glycerolBackbone"
                                                  |+| lookupBlockComp "phosphate"
                                                  |+| lookupBlockComp "glycerolphosphate"
                                                  |+| getFormula x
                                                  |+| getFormula y
                            (KnownSnPGP  x y)     -> lookupBlockComp "glycerolBackbone"
                                                  |+| lookupBlockComp "phosphate"
                                                  |+| lookupBlockComp "glycerolphosphate"
                                                  |+| getFormula x
                                                  |+| getFormula y

instance MolecularFormulae PI where
    getFormula lipid = case lipid of
                            (ClassLevelPI _) -> Nothing
                            (CombinedRadylsPI x) -> lookupBlockComp "glycerolBackbone"
                                                  |+| lookupBlockComp "phosphate"
                                                  |+| lookupBlockComp "inositol"
                                                  |+| getFormula x
                            (UnknownSnPI (x, y))   -> lookupBlockComp "glycerolBackbone"
                                                  |+| lookupBlockComp "phosphate"
                                                  |+| lookupBlockComp "inositol"
                                                  |+| getFormula x
                                                  |+| getFormula y   
                            (KnownSnPI x y)       -> lookupBlockComp "glycerolBackbone"
                                                  |+| lookupBlockComp "phosphate"
                                                  |+| lookupBlockComp "inositol"
                                                  |+| getFormula x
                                                  |+| getFormula y

instance MolecularFormulae PIP where
    getFormula lipid = case lipid of
                            (ClassLevelPIP _)        -> Nothing
                            (CombinedRadylsPIP x _)  -> combinedDiradylComp "inositolPhosphate" x
                            (UnknownSnPIP (x, y) _)  -> diradylComp "inositolPhosphate" x y
                            (KnownSnPIP x y _)       -> diradylComp "inositolPhosphate" x y

instance MolecularFormulae PIP2 where
    getFormula lipid = case lipid of
                            (ClassLevelPIP2 _)       -> Nothing
                            (CombinedRadylsPIP2 x _) -> combinedDiradylComp "inositolDiPhosphate" x
                            (UnknownSnPIP2 (x, y) _) -> diradylComp "inositolDiPhosphate" x y
                            (KnownSnPIP2 x y _)      -> diradylComp "inositolDiPhosphate" x y

instance MolecularFormulae PIP3 where
    getFormula lipid = case lipid of
                            (ClassLevelPIP3 _)       -> Nothing
                            (CombinedRadylsPIP3 x) -> combinedDiradylComp "inositolTriPhosphate" x
                            (UnknownSnPIP3 (x, y)) -> diradylComp "inositolTriPhosphate" x y
                            (KnownSnPIP3 x y)      -> diradylComp "inositolTriPhosphate" x y

instance MolecularFormulae PS where
    getFormula lipid = case lipid of
                            (ClassLevelPS _)       -> Nothing
                            (CombinedRadylsPS x) -> combinedDiradylComp "serine" x
                            (UnknownSnPS (x, y)) -> diradylComp "serine" x y
                            (KnownSnPS x y)      -> diradylComp "serine" x y

instance MolecularFormulae CL where
    getFormula lipid = case lipid of
                            (ClassLevelCL _)          -> Nothing 
                            (CombinedRadylsCL x)      ->  3 |*| lookupBlockComp "glycerolBackbone"
                                                      |+| 2 |*| lookupBlockComp "phosphate"
                                                      |+| lookupBlockComp "hydroxyl"
                                                      |+| getFormula x
                            (UnknownSnCL (x, y, z, w)) ->  3 |*| lookupBlockComp "glycerolBackbone"
                                                      |+| 2 |*| lookupBlockComp "phosphate"
                                                      |+| lookupBlockComp "hydroxyl"
                                                      |+| getFormula x
                                                      |+| getFormula y
                                                      |+| getFormula z
                                                      |+| getFormula w
                            (KnownSnCL  x y z w)  ->  3 |*| lookupBlockComp "glycerolBackbone"
                                                      |+| 2 |*| lookupBlockComp "phosphate"
                                                      |+| lookupBlockComp "hydroxyl"
                                                      |+| getFormula x
                                                      |+| getFormula y
                                                      |+| getFormula z
                                                      |+| getFormula w

instance MolecularFormulae BMP where
    getFormula lipid = case lipid of
                            (ClassLevelBMP  _)        -> Nothing 
                            (CombinedRadylsBMP x)     ->  3 |*| lookupBlockComp "glycerolBackbone"
                                                      |+| 2 |*| lookupBlockComp "phosphate"
                                                      |+| 3 |*|lookupBlockComp "hydroxyl"
                                                      |+| getFormula x
                            (BMP (x, y))              ->  3 |*| lookupBlockComp "glycerolBackbone"
                                                      |+| 2 |*| lookupBlockComp "phosphate"
                                                      |+| 3 |*| lookupBlockComp "hydroxyl"
                                                      |+| getFormula x
                                                      |+| getFormula y

diradylComp headgroup r1 r2 = lookupBlockComp "glycerolBackbone"
                           |+| lookupBlockComp "phosphate"
                           |+| lookupBlockComp headgroup
                           |+| getFormula r1
                           |+| getFormula r2

combinedDiradylComp headgroup r1 = lookupBlockComp "glycerolBackbone"
                                |+| lookupBlockComp "phosphate"
                                |+| lookupBlockComp headgroup
                                |+| getFormula r1


