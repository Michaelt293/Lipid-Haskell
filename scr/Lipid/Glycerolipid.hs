{-|
Module      : Glycerolipid
Description : Glycerolipid data type and instances of Shorthand and 
              Nomenclature defined.
Copyright   : Michael Thomas 
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : experimental
-}

module Lipid.Glycerolipid

import ElementIsotopes
import Lipid.Blocks
import Lipid.Format


data MG = ClassLevelMG       IntegerMass
        | UnknownSn          Radyl
        | Sn1MG              Radyl
        | Sn2MG              Radyl
        | Sn3MG              Radyl
        deriving (Show, Eq, Ord)

data DG = ClassLevelDG       IntegerMass
        | CombinedRadylsDG   CombinedRadyls
        | UnknownDG          (Radyl, Radyl)
        | Sn12DG             { dgSn1 :: Radyl
                             , dgSn2 :: Radyl } 
        | Sn13DG             { dgSn1 :: Radyl
                             , dgSn3 :: Radyl }
        | Sn23DG             { dgSn2 :: Radyl
                             , dgSn3 :: Radyl }
        deriving (Show, Eq, Ord)

data TG = ClassLevelTG       IntegerMass
        | CombinedRadylsTG   CombinedRadyls
        | UnknownSnTG        (Radyl, Radyl, Radyl)
        | KnownSnTG          { tgSn1 :: Radyl
                             , tgSn2 :: Radyl
                             , tgSn3 :: Radyl }
        deriving (Show, Eq, Ord)


instance Shorthand MG where
    showShorthand (ClassLevelMG x)     = "MG (" ++ show x ++ ")"
    showShorthand (UnknownSn x)        = "MG " ++ showShorthand x  
    showShorthand (Sn1MG x)            = "MG " ++ showShorthand x ++ "/0:0/0:0"
    showShorthand (Sn2MG x)            = "MG 0:0/" ++ showShorthand x ++ "/0:0"
    showShorthand (Sn3MG x)            = "MG 0:0/0:0/" ++ showShorthand x
    
instance Nomenclature MG where
    showNnomenclature (ClassLevelMG x) = "MG (" ++ show x ++ ")"
    showNnomenclature (UnknownSn x)    = "MG " ++ showNnomenclature x
    showNnomenclature (Sn1MG x)        = "MG " ++ showNnomenclature x ++ "/0:0/0:0"
    showNnomenclature (Sn2MG x)        = "MG 0:0/" ++ showNnomenclature x ++ "/0:0"
    showNnomenclature (Sn3MG x)        = "MG 0:0/0:0/" ++ showNnomenclature x

instance Shorthand DG where
    showShorthand (ClassLevelDG x)     = "DG (" ++ show x ++ ")"
    showShorthand (CombinedRadylsDG x) = "DG " ++ showShorthand x
    showShorthand (UnknownDG (x,y))    = "DG " ++ fa1 ++ "_" ++ fa2
        where fa1 = showShorthand x
              fa2 = showShorthand y
    showShorthand (Sn12DG x y)         = "DG " ++ sn1 ++ "/" ++ sn2 ++ "/0:0"
        where sn1 = showShorthand x
              sn2 = showShorthand y
    showShorthand (Sn13DG x y)         = "DG " ++ sn1 ++ "/0:0/" ++ sn3
        where sn1 = showShorthand x
              sn3 = showShorthand y
    showShorthand (Sn23DG x y)         = "DG 0:0/" ++ sn2 ++ "/" ++ sn3
        where sn2 = showShorthand x
              sn3 = showShorthand y
 
instance Nomenclature DG where
    showNnomenclature (ClassLevelDG x)     = "DG (" ++ show x ++ ")"
    showNnomenclature (CombinedRadylsDG x) = "DG " ++ showNnomenclature x
    showNnomenclature (UnknownDG (x,y))    = "DG " ++ fa1 ++ "_" ++ fa2
        where fa1 = showNnomenclature x
              fa2 = showNnomenclature y
    showNnomenclature (Sn12DG x y)         = "DG " ++ sn1 ++ "/" ++ sn2 ++ "/0:0"
        where sn1 = showNnomenclature x
              sn2 = showNnomenclature y
    showNnomenclature (Sn13DG x y)         = "DG " ++ sn1 ++ "/0:0/" ++ sn3
        where sn1 = showNnomenclature x
              sn3 = showNnomenclature y
    showNnomenclature (Sn23DG x y)         = "DG 0:0/" ++ sn2 ++ "/" ++ sn3 
        where sn2 = showNnomenclature x
              sn3 = showNnomenclature y

instance Shorthand TG where
    showShorthand (ClassLevelTG x)         = "TG (" ++ show x ++ ")"
    showShorthand (CombinedRadylsTG x)     = "TG " ++ showShorthand x
    showShorthand (UnknownSnTG (x, y, z))  = "TG " ++ fa1 ++ "_" ++ fa2 ++ "_" ++ fa3 
        where fa1 = showShorthand x
              fa2 = showShorthand y
              fa3 = showShorthand z
    showShorthand (KnownSnTG x y z)        = "TG " ++ sn1 ++ "/" ++ sn2 ++ "/" ++ sn3   
        where sn1 = showShorthand x
              sn2 = showShorthand y
              sn3 = showShorthand z
 
instance Nomenclature TG where
    showNnomenclature (ClassLevelTG x)        = "TG (" ++ show x ++ ")"
    showNnomenclature (CombinedRadylsTG x)    = "TG " ++ showNnomenclature x
    showNnomenclature (UnknownSnTG (x, y, z)) = "TG " ++ fa1 ++ "_" ++ fa2 ++ "_" ++ fa3
        where fa1 = showNnomenclature x
              fa2 = showNnomenclature y
              fa3 = showNnomenclature z
    showNnomenclature (KnownSnTG x y z)       = "TG " ++ sn1 ++ "/" ++ sn2 ++ "/" ++ sn3
        where sn1 = showNnomenclature x
              sn2 = showNnomenclature y
              sn3 = showNnomenclature z


