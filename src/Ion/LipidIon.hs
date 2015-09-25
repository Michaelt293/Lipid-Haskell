{-|
Module      : Ion.LipidIon
Description : Adducts and lipid ions are defined. 
Copyright   : Michael Thomas
License     : GPL-3
aintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental
-}

module Ion.LipidIon where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import ElementIsotopes hiding (monoisotopicMass, nominalMass)
import Lipid.Blocks
import Lipid.FattyAcid
import Lipid.Glycerolipid
import Lipid.Glycerophospholipid
import Lipid.Lysoglycerophospholipid
import Formula.Blocks
import Formula.FattyAcid
import Formula.Glycerolipid
import Formula.Glycerophospholipid
import Formula.Lysoglycerophospholipid



type ProtonationState = Integer 

data Adduct =  H'  | Li' | F'  | Na'  | Mg' |
               Cl' | K'  | Ca' | Br'  | Ag' | 
               I'  | Cs' | Ba' | HCO2 | OAc
               deriving (Show, Read, Eq, Ord)

data LipidIon = FAion    FA    ProtonationState [(Adduct, Integer)]
              | MGion    MG    ProtonationState [(Adduct, Integer)]
              | DGion    DG    ProtonationState [(Adduct, Integer)]
              | TGion    TG    ProtonationState [(Adduct, Integer)]
              | PAion    PA    ProtonationState [(Adduct, Integer)] 
              | PCion    PC    ProtonationState [(Adduct, Integer)] 
              | PEion    PE    ProtonationState [(Adduct, Integer)] 
              | PGion    PG    ProtonationState [(Adduct, Integer)] 
              | PGPion   PGP   ProtonationState [(Adduct, Integer)]
              | PIion    PI    ProtonationState [(Adduct, Integer)]
              | PIPion   PIP   ProtonationState [(Adduct, Integer)]
              | PIP2ion  PIP2  ProtonationState [(Adduct, Integer)]
              | PIP3ion  PIP3  ProtonationState [(Adduct, Integer)]
              | PSion    PS    ProtonationState [(Adduct, Integer)]
              | CLion    CL    ProtonationState [(Adduct, Integer)]
              | BMPion   BMP   ProtonationState [(Adduct, Integer)]
              | LPAion   LPA   ProtonationState [(Adduct, Integer)]
              | LPCion   LPC   ProtonationState [(Adduct, Integer)]
              | LPEion   LPE   ProtonationState [(Adduct, Integer)]
              | LPGion   LPG   ProtonationState [(Adduct, Integer)]
              | LPGPion  LPGP  ProtonationState [(Adduct, Integer)]
              | LPIion   LPI   ProtonationState [(Adduct, Integer)]
              | LPIPion  LPIP  ProtonationState [(Adduct, Integer)]
              | LPIP2ion LPIP2 ProtonationState [(Adduct, Integer)]
              | LPIP3ion LPIP3 ProtonationState [(Adduct, Integer)]
              | LPSion   LPS   ProtonationState [(Adduct, Integer)]
              | LCLion   LCL   ProtonationState [(Adduct, Integer)]
              deriving (Show, Eq, Ord)

adductComp = Map.fromList
    [ (H',   Just (MolecularFormula (Map.fromList [(H,  1)])))
    , (Li',  Just (MolecularFormula (Map.fromList [(Li, 1)])))
    , (F',   Just (MolecularFormula (Map.fromList [(F,  1)])))
    , (Na',  Just (MolecularFormula (Map.fromList [(Na, 1)])))
    , (Mg',  Just (MolecularFormula (Map.fromList [(Mg, 1)])))
    , (Cl',  Just (MolecularFormula (Map.fromList [(Cl, 1)])))
    , (K',   Just (MolecularFormula (Map.fromList [(K,  1)])))
    , (Ca',  Just (MolecularFormula (Map.fromList [(Ca, 1)])))
    , (Br',  Just (MolecularFormula (Map.fromList [(Br, 1)])))
    , (Ag',  Just (MolecularFormula (Map.fromList [(Ag, 1)])))
    , (I',   Just (MolecularFormula (Map.fromList [(I,  1)])))
    , (Cs',  Just (MolecularFormula (Map.fromList [(Cs, 1)])))
    , (Ba',  Just (MolecularFormula (Map.fromList [(Ba, 1)])))
    , (HCO2, Just (MolecularFormula (Map.fromList [(C,  1),
                                                   (O,  2),
                                                   (H,  1)])))
    , (OAc,  Just (MolecularFormula (Map.fromList [(C,  2),
                                                   (O,  2),
                                                   (H,  3)])))
    ]

adductCharge =  Map.fromList
    [ (H',    1 )
    , (Li',   1 )
    , (F',   -1 ) 
    , (Na',   1 )
    , (Mg',   2 )
    , (Cl',  -1 ) 
    , (K',    1 )
    , (Ca',   2 )
    , (Br',  -1 )
    , (Ag',   1 )
    , (I',   -1 ) 
    , (Cs',   1 )
    , (Ba',   2 )
    , (HCO2,  1 )
    , (OAc,   1 )
    ]

instance Shorthand Adduct where
     showShorthand x = case x of 
                            H'   -> "H"
                            Li'  -> "Li"
                            F'   -> "F"
                            Na'  -> "Na"
                            Mg'  -> "Mg"
                            Cl'  -> "Cl"
                            K'   -> "K"
                            Ca'  -> "Ca"
                            Br'  -> "Br"
                            Ag'  -> "Ag"
                            I'   -> "I"
                            Cs'  -> "Cs"
                            Ba'  -> "Ba"
                            HCO2 -> "HCO2"
                            OAc  -> "OAc"

instance Shorthand LipidIon where
    showShorthand x = case x of
                           (FAion l d a)    -> renderLipidIon l d a
                           (MGion l d a)    -> renderLipidIon l d a
                           (DGion l d a)    -> renderLipidIon l d a
                           (TGion l d a)    -> renderLipidIon l d a
                           (PAion l d a)    -> renderLipidIon l d a
                           (PCion l d a)    -> renderLipidIon l d a
                           (PEion l d a)    -> renderLipidIon l d a
                           (PGion l d a)    -> renderLipidIon l d a
                           (PGPion l d a)   -> renderLipidIon l d a
                           (PIion l d a)    -> renderLipidIon l d a
                           (PIPion l d a)   -> renderLipidIon l d a 
                           (PIP2ion l d a)  -> renderLipidIon l d a 
                           (PIP3ion l d a)  -> renderLipidIon l d a  
                           (PSion l d a)    -> renderLipidIon l d a
                           (CLion l d a)    -> renderLipidIon l d a
                           (BMPion l d a)   -> renderLipidIon l d a
                           (LPAion l d a)   -> renderLipidIon l d a
                           (LPCion l d a)   -> renderLipidIon l d a
                           (LPEion l d a)   -> renderLipidIon l d a
                           (LPGion l d a)   -> renderLipidIon l d a
                           (LPGPion l d a)  -> renderLipidIon l d a  
                           (LPIion l d a)   -> renderLipidIon l d a
                           (LPIPion l d a)  -> renderLipidIon l d a 
                           (LPIP2ion l d a) -> renderLipidIon l d a
                           (LPIP3ion l d a) -> renderLipidIon l d a
                           (LPSion l d a)   -> renderLipidIon l d a
                           (LCLion l d a)   -> renderLipidIon l d a


renderLipidIon :: Shorthand a => a -> Integer -> [(Adduct, ProtonationState)] -> [Char]
renderLipidIon l p as = "[" ++ showShorthand l ++
                        pstate ++ adducts ++ "]" ++ showCharge
    where pstate 
              | p ==  0 = ""
              | p == -1 = " - " ++ "H"
              | p ==  1 = " + " ++ "H"
              | p <  -1 = " - " ++ (show . abs) p ++ "H"
              | p >   1 = " + " ++ (show . abs) p ++ "H"
          adducts
              | length as > 0 = " + " ++ List.intercalate "+"
                                (map (\(x, y) -> if y == 1 then showShorthand x 
                                                           else show y ++ showShorthand x)
                                                           as)
              | otherwise     = ""
          showCharge
              | charge' ==  1 = "+"
              | charge' >   1 = show p ++ "+"
              | charge' == -1 = "-"
              | charge' <  -1 = (show . abs) p ++ "-"
              where charge'   = charge p as

charge :: ProtonationState -> [(Adduct, ProtonationState)] -> ProtonationState
charge p as = p + sum (map (\(a, n) -> (lookupAdductCharge a) * n) as)

lipidIonCharge :: LipidIon -> ProtonationState
lipidIonCharge lipidIon =
    case lipidIon of
         (FAion    _ p as) -> charge p as
         (MGion    _ p as) -> charge p as
         (DGion    _ p as) -> charge p as
         (TGion    _ p as) -> charge p as
         (PAion    _ p as) -> charge p as
         (PCion    _ p as) -> charge p as
         (PEion    _ p as) -> charge p as
         (PGion    _ p as) -> charge p as
         (PGPion   _ p as) -> charge p as
         (PIion    _ p as) -> charge p as
         (PIPion   _ p as) -> charge p as
         (PIP2ion  _ p as) -> charge p as
         (PIP3ion  _ p as) -> charge p as
         (PSion    _ p as) -> charge p as
         (CLion    _ p as) -> charge p as
         (BMPion   _ p as) -> charge p as
         (LPAion   _ p as) -> charge p as
         (LPCion   _ p as) -> charge p as
         (LPEion   _ p as) -> charge p as
         (LPGion   _ p as) -> charge p as
         (LPGPion  _ p as) -> charge p as
         (LPIion   _ p as) -> charge p as
         (LPIPion  _ p as) -> charge p as
         (LPIP2ion _ p as) -> charge p as
         (LPIP3ion _ p as) -> charge p as
         (LPSion   _ p as) -> charge p as
         (LCLion   _ p as) -> charge p as
                           
lookupAdductComp :: Adduct -> Maybe MolecularFormula
lookupAdductComp sym = find sym adductComp

lookupAdductCharge :: Adduct -> ProtonationState
lookupAdductCharge sym = find sym adductCharge

