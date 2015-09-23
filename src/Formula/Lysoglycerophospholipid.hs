{-|
Module      : Formula.Lysoglycerophospholipid
Description : Instances of MolecularFormula for lysoglycerophospholipid
              data types provided.
Copyright   : Michael Thomas 
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental
-}

module Formula.Lysoglycerophospholipid where


import Lipid.Lysoglycerophospholipid
import Formula.Blocks
 
instance MolecularFormulae LPA where
    getFormula lipid = case lipid of
                            (UnknownSnLPA x) -> lysoComp "proton" x
                            (Sn1LPA x) -> lysoComp "proton" x
                            (Sn2LPA x) -> lysoComp "proton" x 
          
instance MolecularFormulae LPC where
    getFormula lipid = case lipid of
                            (UnknownSnLPC x) -> lysoComp "choline" x
                            (Sn1LPC x) -> lysoComp "choline" x    
                            (Sn2LPC x) -> lysoComp "choline" x        

instance MolecularFormulae LPE where
    getFormula lipid = case lipid of
                            (UnknownSnLPE x) -> lysoComp "ethanolamine" x
                            (Sn1LPE x) -> lysoComp "ethanolamine" x      
                            (Sn2LPE x) -> lysoComp "ethanolamine" x           

instance MolecularFormulae LPG where
    getFormula lipid = case lipid of
                            (UnknownSnLPG x) -> lysoComp "glycerol" x
                            (Sn1LPG x) -> lysoComp "glycerol" x      
                            (Sn2LPG x) -> lysoComp "glycerol" x             

instance MolecularFormulae LPGP where
    getFormula lipid = case lipid of 
                            (UnknownSnLPGP x) -> lysoComp "glycerolPhosphate" x
                            (Sn1LPGP x) -> lysoComp "glycerolPhosphate" x     
                            (Sn2LPGP x) -> lysoComp "glycerolPhosphate" x          
instance MolecularFormulae LPI where
    getFormula lipid = case lipid of
                            (UnknownSnLPI x) -> lysoComp "inositol" x       
                            (Sn1LPI x) -> lysoComp "inositol" x
                            (Sn2LPI x) -> lysoComp "inositol" x           

instance MolecularFormulae LPIP where
    getFormula lipid = case lipid of 
                            (UnknownSnLPIP x _) -> lysoComp "inositolPhosphate" x 
                            (Sn1LPIP x _) -> lysoComp "inositolPhosphate" x     
                            (Sn2LPIP x _) -> lysoComp "inositolPhosphate" x          

instance MolecularFormulae LPIP2 where
    getFormula lipid = case lipid of
                            (UnknownSnLPIP2 x _) -> lysoComp "inositolDiPhosphate" x
                            (Sn1LPIP2 x _) -> lysoComp "inositolDiPhosphate" x     
                            (Sn2LPIP2 x _) -> lysoComp "inositolDiPhosphate" x          

instance MolecularFormulae LPIP3 where
    getFormula lipid = case lipid of
                            (UnknownSnLPIP3 x) -> lysoComp "inositolTriPhosphate" x
                            (Sn1LPIP3 x) -> lysoComp "inositolTriPhosphate" x    
                            (Sn2LPIP3 x) -> lysoComp "inositolTriPhosphate" x         

instance MolecularFormulae LPS where
    getFormula lipid = case lipid of
                            (UnknownSnLPS x) -> lysoComp "serine" x       
                            (Sn1LPS x) -> lysoComp "serine" x
                            (Sn2LPS x) -> lysoComp "serine" x           

instance MolecularFormulae LCL where
    getFormula lipid = case lipid of
                            (UnknownSnLCL (x, y, z)) -> lysoCLComp "proton" x y z
                            (Sn1LCL x y z) -> lysoCLComp "proton" x y z       
                            (Sn2LCL x y z) -> lysoCLComp "proton" x y z


lysoComp headgroup r = lookupBlockComp "glycerolBackbone"
                    |+| lookupBlockComp "phosphate"
                    |+| lookupBlockComp "hydroxyl"
                    |+| lookupBlockComp headgroup
                    |+| getFormula r

lysoCLComp headgroup r1 r2 r3 =  3 |*| lookupBlockComp "glycerolBackbone"
                            |+| 2 |*| lookupBlockComp "phosphate"
                            |+| 2 |*|lookupBlockComp "hydroxyl"
                            |+| getFormula r1
                            |+| getFormula r2
                            |+| getFormula r3
