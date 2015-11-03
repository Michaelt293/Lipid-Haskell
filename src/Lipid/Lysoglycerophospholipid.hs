{-|
Module      : Lipid.Lysoglycerophospholipid
Description : Lysoglycerophospholipid data type and instances of Shorthand and
              Nomenclature defined.
Copyright   : Michael Thomas
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental
-}

module Lipid.Lysoglycerophospholipid where

import ElementIsotopes
import Lipid.Blocks
import Lipid.Format
import Lipid.Glycerophospholipid

data LPA   = UnknownSnLPA       Radyl
           | Sn1LPA             Radyl
           | Sn2LPA             Radyl
           deriving (Show, Eq, Ord)

data LPC   = UnknownSnLPC       Radyl
           | Sn1LPC             Radyl
           | Sn2LPC             Radyl
           deriving (Show, Eq, Ord)

data LPE   = UnknownSnLPE       Radyl
           | Sn1LPE             Radyl
           | Sn2LPE             Radyl
           deriving (Show, Eq, Ord)

data LPG   = UnknownSnLPG       Radyl
           | Sn1LPG             Radyl
           | Sn2LPG             Radyl
           deriving (Show, Eq, Ord)

data LPGP  = UnknownSnLPGP      Radyl
           | Sn1LPGP            Radyl
           | Sn2LPGP            Radyl
           deriving (Show, Eq, Ord)

data LPI   = UnknownSnLPI       Radyl
           | Sn1LPI             Radyl
           | Sn2LPI             Radyl
           deriving (Show, Eq, Ord)

data LPIP  = UnknownSnLPIP      Radyl (Maybe PhosphatePosition)
           | Sn1LPIP            Radyl (Maybe PhosphatePosition)
           | Sn2LPIP            Radyl (Maybe PhosphatePosition)
           deriving (Show, Eq, Ord)

data LPIP2 = UnknownSnLPIP2     Radyl ( Maybe PhosphatePosition
                                      , Maybe PhosphatePosition )
           | Sn1LPIP2           Radyl ( Maybe PhosphatePosition
                                      , Maybe PhosphatePosition )
           | Sn2LPIP2           Radyl (Maybe PhosphatePosition
                                      , Maybe PhosphatePosition )

           deriving (Show, Eq, Ord)

data LPIP3 = UnknownSnLPIP3     Radyl
           | Sn1LPIP3           Radyl
           | Sn2LPIP3           Radyl
           deriving (Show, Eq, Ord)

data LPS   = UnknownSnLPS       Radyl
           | Sn1LPS             Radyl
           | Sn2LPS             Radyl
           deriving (Show, Eq, Ord)

data LCL   = UnknownSnLCL      Radyl Radyl Radyl
           | Sn1LCL            Radyl Radyl Radyl
           | Sn2LCL            Radyl Radyl Radyl
           deriving (Show, Eq, Ord)


instance Shorthand LPA where
    showShorthand (UnknownSnLPA r) = "LPA " ++ showShorthand r ++ "_0:0"
    showShorthand (Sn1LPA r)       = "LPA " ++ showShorthand r ++ "/0:0"
    showShorthand (Sn2LPA r)       = "LPA 0:0/" ++ showShorthand r 

instance Nomenclature LPA where
    showNnomenclature (UnknownSnLPA r) = "LPA " ++ showNnomenclature r ++ "_0:0"
    showNnomenclature (Sn1LPA r)       = "LPA " ++ showNnomenclature r ++ "/0:0"
    showNnomenclature (Sn2LPA r)       = "LPA 0:0/" ++ showNnomenclature r 

instance Shorthand LPC where
    showShorthand (UnknownSnLPC r) = "LPC " ++ showShorthand r ++ "_0:0"
    showShorthand (Sn1LPC r)       = "LPC " ++ showShorthand r ++ "/0:0"
    showShorthand (Sn2LPC r)       = "LPC 0:0/" ++ showShorthand r 

instance Nomenclature LPC where
    showNnomenclature (UnknownSnLPC r) = "LPC " ++ showNnomenclature r ++ "_0:0"
    showNnomenclature (Sn1LPC r)       = "LPC " ++ showNnomenclature r ++ "/0:0"
    showNnomenclature (Sn2LPC r)       = "LPC 0:0/" ++ showNnomenclature r 

instance Shorthand LPE where
    showShorthand (UnknownSnLPE r) = "LPE " ++ showShorthand r ++ "_0:0"
    showShorthand (Sn1LPE r)       = "LPE " ++ showShorthand r ++ "/0:0"
    showShorthand (Sn2LPE r)       = "LPE 0:0/" ++ showShorthand r 

instance Nomenclature LPE where
    showNnomenclature (UnknownSnLPE r) = "LPE " ++ showNnomenclature r ++ "_0:0"
    showNnomenclature (Sn1LPE r)       = "LPE " ++ showNnomenclature r ++ "/0:0"
    showNnomenclature (Sn2LPE r)       = "LPE 0:0/" ++ showNnomenclature r 

instance Shorthand LPG where
    showShorthand (UnknownSnLPG r) = "LPG " ++ showShorthand r ++ "_0:0"
    showShorthand (Sn1LPG r)       = "LPG " ++ showShorthand r ++ "/0:0"
    showShorthand (Sn2LPG r)       = "LPG 0:0/" ++ showShorthand r 

instance Nomenclature LPG where
    showNnomenclature (UnknownSnLPG r) = "LPG " ++ showNnomenclature r ++ "_0:0"
    showNnomenclature (Sn1LPG r)       = "LPG " ++ showNnomenclature r ++ "/0:0"
    showNnomenclature (Sn2LPG r)       = "LPG 0:0/" ++ showNnomenclature r 

instance Shorthand LPGP where
    showShorthand (UnknownSnLPGP r) = "LPGP " ++ showShorthand r ++ "_0:0"
    showShorthand (Sn1LPGP r)       = "LPGP " ++ showShorthand r ++ "/0:0"
    showShorthand (Sn2LPGP r)       = "LPGP 0:0/" ++ showShorthand r 

instance Nomenclature LPGP where
    showNnomenclature (UnknownSnLPGP r) = "LPGP " ++ showNnomenclature r ++ "_0:0"
    showNnomenclature (Sn1LPGP r)       = "LPGP " ++ showNnomenclature r ++ "/0:0"
    showNnomenclature (Sn2LPGP r)       = "LPGP 0:0/" ++ showNnomenclature r 

instance Shorthand LPI where
    showShorthand (UnknownSnLPI r) = "LPI " ++ showShorthand r ++ "_0:0"
    showShorthand (Sn1LPI r)       = "LPI " ++ showShorthand r ++ "/0:0"
    showShorthand (Sn2LPI r)       = "LPI 0:0/" ++ showShorthand r 
 
instance Nomenclature LPI where
    showNnomenclature (UnknownSnLPI r) = "LPI " ++ showNnomenclature r ++ "_0:0"
    showNnomenclature (Sn1LPI r)       = "LPI " ++ showNnomenclature r ++ "/0:0"
    showNnomenclature (Sn2LPI r)       = "LPI 0:0/" ++ showNnomenclature r 

instance Shorthand LPIP where
    showShorthand (UnknownSnLPIP r p) = "LPI" ++ renderPO4 p ++ " " ++ showShorthand r ++ "_0:0"
    showShorthand (Sn1LPIP r p)       = "LPI" ++ renderPO4 p ++ " " ++ showShorthand r ++ "/0:0"
    showShorthand (Sn2LPIP r p)       = "LPI"  ++ renderPO4 p ++ " 0:0/" ++ showShorthand r 

instance Nomenclature LPIP where
    showNnomenclature (UnknownSnLPIP r p) = "LPI" ++ renderPO4 p ++ " " ++ showNnomenclature r ++ "_0:0"
    showNnomenclature (Sn1LPIP r p)       = "LPI" ++ renderPO4 p ++ " " ++ showNnomenclature r ++ "/0:0"
    showNnomenclature (Sn2LPIP r p)       = "LPI"  ++ renderPO4 p ++ " 0:0/" ++ showNnomenclature r 

instance Shorthand LPIP2 where
    showShorthand (UnknownSnLPIP2 r p) = "LPIP2" ++ renderPO4s p ++ showShorthand r ++ "_0:0"
    showShorthand (Sn1LPIP2 r p)       = "LPIP2" ++ renderPO4s p ++ " " ++ showShorthand r ++ "/0:0"
    showShorthand (Sn2LPIP2 r p)       = "LPIP2" ++ renderPO4s p ++ " 0:0/" ++ showShorthand r

instance Nomenclature LPIP2 where
    showNnomenclature (UnknownSnLPIP2 r p) = "LPIP2" ++ renderPO4s p ++ showNnomenclature r ++ "_0:0"
    showNnomenclature (Sn1LPIP2 r p)       = "LPIP2" ++ renderPO4s p ++ " " ++ showNnomenclature r ++ "/0:0"
    showNnomenclature (Sn2LPIP2 r p)       = "LPIP2" ++ renderPO4s p ++ " 0:0/" ++ showNnomenclature r

instance Shorthand LPIP3 where
    showShorthand (UnknownSnLPIP3 r) = "LPIP3 " ++ showShorthand r ++ "_0:0"
    showShorthand (Sn1LPIP3 r)       = "LPIP3 " ++ showShorthand r ++ "/0:0"
    showShorthand (Sn2LPIP3 r)       = "LPIP3 0:0/" ++ showShorthand r 

instance Nomenclature LPIP3 where
    showNnomenclature (UnknownSnLPIP3 r) = "LPIP3 " ++ showNnomenclature r ++ "_0:0"
    showNnomenclature (Sn1LPIP3 r)       = "LPIP3 " ++ showNnomenclature r ++ "/0:0"
    showNnomenclature (Sn2LPIP3 r)       = "LPIP3 0:0/" ++ showNnomenclature r 

instance Shorthand LPS where
    showShorthand (UnknownSnLPS r) = "LPS " ++ showShorthand r ++ "_0:0"
    showShorthand (Sn1LPS r)       = "LPS " ++ showShorthand r ++ "/0:0"
    showShorthand (Sn2LPS r)       = "LPS 0:0/" ++ showShorthand r 

instance Nomenclature LPS where
    showNnomenclature (UnknownSnLPS r) = "LPS " ++ showNnomenclature r ++ "_0:0"
    showNnomenclature (Sn1LPS r)       = "LPS " ++ showNnomenclature r ++ "/0:0"
    showNnomenclature (Sn2LPS r)       = "LPS 0:0/" ++ showNnomenclature r 

instance Shorthand LCL where
    showShorthand (UnknownSnLCL r1 r2 r3) = renderUnknownSnLCL showShorthand r1 r2 r3
    showShorthand (Sn1LCL r1 r2 r3)       = renderKnownSnLCL showShorthand "sn1" r1 r2 r3
    showShorthand (Sn2LCL r1 r2 r3)       = renderKnownSnLCL showShorthand "sn2" r1 r2 r3

instance Nomenclature LCL where
    showNnomenclature (UnknownSnLCL r1 r2 r3) = renderUnknownSnLCL showNnomenclature r1 r2 r3
    showNnomenclature (Sn1LCL r1 r2 r3)       = renderKnownSnLCL showNnomenclature "sn1" r1 r2 r3
    showNnomenclature (Sn2LCL r1 r2 r3)       = renderKnownSnLCL showNnomenclature "sn2" r1 r2 r3

renderUnknownSnLCL f r1 r2 r3 = "LCL " ++ f r1 ++ "_" ++ f r2 ++ "_" ++ f r3
  
renderKnownSnLCL f sn r1 r2 r3 =
  case sn of
    "sn1" -> "LCL " ++ f r1 ++ "/0:0/" ++ f r2 ++ "/" ++ f r3
    "sn2" -> "LCL 0:0/" ++ f r1 ++ "/" ++ f r2 ++ "/" ++ f r3
