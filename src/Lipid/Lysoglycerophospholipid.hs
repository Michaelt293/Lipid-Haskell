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

data LPIP2 = UnknownSnLPIP2     Radyl (Maybe PhosphatePosition, Maybe PhosphatePosition)
           | Sn1LPIP2           Radyl (Maybe PhosphatePosition, Maybe PhosphatePosition)
           | Sn2LPIP2           Radyl (Maybe PhosphatePosition, Maybe PhosphatePosition)

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
    showShorthand (UnknownSnLPA x)   = "LPA " ++ showShorthand x ++ "_0:0"
    showShorthand (Sn1LPA x)         = "LPA " ++ showShorthand x ++ "/0:0"
    showShorthand (Sn2LPA x)         = "LPA 0:0/" ++ showShorthand x 

instance Nomenclature LPA where
    showNnomenclature (UnknownSnLPA x)   = "LPA " ++ showNnomenclature x ++ "_0:0"
    showNnomenclature (Sn1LPA x)         = "LPA " ++ showNnomenclature x ++ "/0:0"
    showNnomenclature (Sn2LPA x)         = "LPA 0:0/" ++ showNnomenclature x 

instance Shorthand LPC where
    showShorthand (UnknownSnLPC x)   = "LPC " ++ showShorthand x ++ "_0:0"
    showShorthand (Sn1LPC x)         = "LPC " ++ showShorthand x ++ "/0:0"
    showShorthand (Sn2LPC x)         = "LPC 0:0/" ++ showShorthand x 

instance Nomenclature LPC where
    showNnomenclature (UnknownSnLPC x)   = "LPC " ++ showNnomenclature x ++ "_0:0"
    showNnomenclature (Sn1LPC x)         = "LPC " ++ showNnomenclature x ++ "/0:0"
    showNnomenclature (Sn2LPC x)         = "LPC 0:0/" ++ showNnomenclature x 

instance Shorthand LPE where
    showShorthand (UnknownSnLPE x)   = "LPE " ++ showShorthand x ++ "_0:0"
    showShorthand (Sn1LPE x)         = "LPE " ++ showShorthand x ++ "/0:0"
    showShorthand (Sn2LPE x)         = "LPE 0:0/" ++ showShorthand x 

instance Nomenclature LPE where
    showNnomenclature (UnknownSnLPE x)   = "LPE " ++ showNnomenclature x ++ "_0:0"
    showNnomenclature (Sn1LPE x)         = "LPE " ++ showNnomenclature x ++ "/0:0"
    showNnomenclature (Sn2LPE x)         = "LPE 0:0/" ++ showNnomenclature x 

instance Shorthand LPG where
    showShorthand (UnknownSnLPG x)   = "LPG " ++ showShorthand x ++ "_0:0"
    showShorthand (Sn1LPG x)         = "LPG " ++ showShorthand x ++ "/0:0"
    showShorthand (Sn2LPG x)         = "LPG 0:0/" ++ showShorthand x 

instance Nomenclature LPG where
    showNnomenclature (UnknownSnLPG x)   = "LPG " ++ showNnomenclature x ++ "_0:0"
    showNnomenclature (Sn1LPG x)         = "LPG " ++ showNnomenclature x ++ "/0:0"
    showNnomenclature (Sn2LPG x)         = "LPG 0:0/" ++ showNnomenclature x 

instance Shorthand LPGP where
    showShorthand (UnknownSnLPGP x)   = "LPGP " ++ showShorthand x ++ "_0:0"
    showShorthand (Sn1LPGP x)         = "LPGP " ++ showShorthand x ++ "/0:0"
    showShorthand (Sn2LPGP x)         = "LPGP 0:0/" ++ showShorthand x 

instance Nomenclature LPGP where
    showNnomenclature (UnknownSnLPGP x)   = "LPGP " ++ showNnomenclature x ++ "_0:0"
    showNnomenclature (Sn1LPGP x)         = "LPGP " ++ showNnomenclature x ++ "/0:0"
    showNnomenclature (Sn2LPGP x)         = "LPGP 0:0/" ++ showNnomenclature x 

instance Shorthand LPI where
    showShorthand (UnknownSnLPI x)   = "LPI " ++ showShorthand x ++ "_0:0"
    showShorthand (Sn1LPI x)         = "LPI " ++ showShorthand x ++ "/0:0"
    showShorthand (Sn2LPI x)         = "LPI 0:0/" ++ showShorthand x 
 
instance Nomenclature LPI where
    showNnomenclature (UnknownSnLPI x)   = "LPI " ++ showNnomenclature x ++ "_0:0"
    showNnomenclature (Sn1LPI x)         = "LPI " ++ showNnomenclature x ++ "/0:0"
    showNnomenclature (Sn2LPI x)         = "LPI 0:0/" ++ showNnomenclature x 

instance Shorthand LPIP where
    showShorthand (UnknownSnLPIP x p)   = "LPI" ++ po4 ++ " " ++ showShorthand x ++ "_0:0"
        where po4 = case p of
                        Nothing  -> ""
                        (Just p') -> wrapBrackets $ showShorthand p'
    showShorthand (Sn1LPIP x p)         = "LPI" ++ po4 ++ " " ++ showShorthand x ++ "/0:0"
        where po4 = case p of
                        Nothing  -> ""
                        (Just p') -> wrapBrackets $ showShorthand p'
    showShorthand (Sn2LPIP x p)         = "LPI"  ++ po4 ++ " 0:0/" ++ showShorthand x 
        where po4 = case p of
                        Nothing  -> ""
                        (Just p') -> wrapBrackets $ showShorthand p'

instance Nomenclature LPIP where
    showNnomenclature (UnknownSnLPIP x p)   = "LPI" ++ po4 ++ " " ++ showNnomenclature x ++ "_0:0"
        where po4 = case p of
                        Nothing  -> ""
                        (Just p') -> wrapBrackets $ showShorthand p'
    showNnomenclature (Sn1LPIP x p)         = "LPI" ++ po4 ++ " " ++ showNnomenclature x ++ "/0:0"
        where po4 = case p of
                        Nothing  -> ""
                        (Just p') -> wrapBrackets $ showShorthand p'
    showNnomenclature (Sn2LPIP x p)         = "LPI"  ++ po4 ++ " 0:0/" ++ showNnomenclature x 
        where po4 = case p of
                        Nothing  -> ""
                        (Just p') -> wrapBrackets $ showShorthand p'

instance Shorthand LPIP2 where
    showShorthand (UnknownSnLPIP2 x p)  = "LPIP2" ++ po4 ++ showShorthand x ++ "_0:0"
        where po4 = case p of
                        (Nothing, _)  -> ""
                        (_, Nothing)  -> ""
                        (Just p1, Just p2) -> wrapBrackets $ showShorthand p1 ++ "," ++ showShorthand p2
    showShorthand (Sn1LPIP2 x p)  = "LPIP2" ++ po4 ++ " " ++ showShorthand x ++ "/0:0"
        where po4 = case p of
                        (Nothing, _)  -> ""
                        (_, Nothing)  -> ""
                        (Just p1, Just p2) -> wrapBrackets $ showShorthand p1 ++ "," ++ showShorthand p2
    showShorthand (Sn2LPIP2 x p) = "LPIP2" ++ po4 ++ " 0:0/" ++ showShorthand x
        where po4 = case p of
                        (Nothing, _)  -> ""
                        (_, Nothing)  -> ""
                        (Just p1, Just p2) -> wrapBrackets $ showShorthand p1 ++ "," ++ showShorthand p2

instance Nomenclature LPIP2 where
    showNnomenclature (UnknownSnLPIP2 x p)  = "LPIP2" ++ po4 ++ showNnomenclature x ++ "_0:0"
        where po4 = case p of
                        (Nothing, _)  -> ""
                        (_, Nothing)  -> ""
                        (Just p1, Just p2) -> wrapBrackets $ showShorthand p1 ++ "," ++ showShorthand p2
    showNnomenclature (Sn1LPIP2 x p)  = "LPIP2" ++ po4 ++ " " ++ showNnomenclature x ++ "/0:0"
        where po4 = case p of
                        (Nothing, _)  -> ""
                        (_, Nothing)  -> ""
                        (Just p1, Just p2) -> wrapBrackets $ showShorthand p1 ++ "," ++ showShorthand p2
    showNnomenclature (Sn2LPIP2 x p) = "LPIP2" ++ po4 ++ " 0:0/" ++ showNnomenclature x
        where po4 = case p of
                        (Nothing, _)  -> ""
                        (_, Nothing)  -> ""
                        (Just p1, Just p2) -> wrapBrackets $ showShorthand p1 ++ "," ++ showShorthand p2

instance Shorthand LPIP3 where
    showShorthand (UnknownSnLPIP3 x)   = "LPIP3 " ++ showShorthand x ++ "_0:0"
    showShorthand (Sn1LPIP3 x)         = "LPIP3 " ++ showShorthand x ++ "/0:0"
    showShorthand (Sn2LPIP3 x)         = "LPIP3 0:0/" ++ showShorthand x 


instance Nomenclature LPIP3 where
    showNnomenclature (UnknownSnLPIP3 x)   = "LPIP3 " ++ showNnomenclature x ++ "_0:0"
    showNnomenclature (Sn1LPIP3 x)         = "LPIP3 " ++ showNnomenclature x ++ "/0:0"
    showNnomenclature (Sn2LPIP3 x)         = "LPIP3 0:0/" ++ showNnomenclature x 

instance Shorthand LPS where
    showShorthand (UnknownSnLPS x)   = "LPS " ++ showShorthand x ++ "_0:0"
    showShorthand (Sn1LPS x)         = "LPS " ++ showShorthand x ++ "/0:0"
    showShorthand (Sn2LPS x)         = "LPS 0:0/" ++ showShorthand x 

instance Nomenclature LPS where
    showNnomenclature (UnknownSnLPS x)   = "LPS " ++ showNnomenclature x ++ "_0:0"
    showNnomenclature (Sn1LPS x)         = "LPS " ++ showNnomenclature x ++ "/0:0"
    showNnomenclature (Sn2LPS x)         = "LPS 0:0/" ++ showNnomenclature x 

instance Shorthand LCL where
    showShorthand (UnknownSnLCL x y z)   = "LCL " ++ fa1 ++ "_0:0_" ++ fa2 ++ "_" ++ fa3 
        where fa1 = showShorthand x
              fa2 = showShorthand y
              fa3 = showShorthand z
    showShorthand (Sn1LCL x y z)       = "CL " ++ sn1 ++ "/0:0/" ++ sn1' ++ "/" ++ sn2'
        where sn1  = showShorthand x
              sn1' = showShorthand y
              sn2' = showShorthand z
    showShorthand (Sn2LCL x y z)       = "CL 0:0/" ++ sn2 ++ "/" ++ sn1' ++ "/" ++ sn2'
        where sn2  = showShorthand x
              sn1' = showShorthand y
              sn2' = showShorthand z

instance Nomenclature LCL where
    showNnomenclature (UnknownSnLCL x y z)   = "LCL " ++ fa1 ++ "_0:0_" ++ fa2 ++ "_" ++ fa3 
        where fa1 = showNnomenclature x
              fa2 = showNnomenclature y
              fa3 = showNnomenclature z
    showNnomenclature (Sn1LCL x y z)       = "CL " ++ sn1 ++ "/0:0/" ++ sn1' ++ "/" ++ sn2'
        where sn1  = showNnomenclature x
              sn1' = showNnomenclature y
              sn2' = showNnomenclature z
    showNnomenclature (Sn2LCL x y z)       = "CL 0:0/" ++ sn2 ++ "/" ++ sn1' ++ "/" ++ sn2'
        where sn2  = showNnomenclature x
              sn1' = showNnomenclature y
              sn2' = showNnomenclature z 

