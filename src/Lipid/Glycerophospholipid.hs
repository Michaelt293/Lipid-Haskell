{-|
Module      : Lipid.Glycerophospholipid
Description : Glycerophospholipid data type and instances of Shorthand and
              Nomenclature defined.
Copyright   : Michael Thomas
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental
-}

module Lipid.Glycerophospholipid where

import ElementIsotopes
import Lipid.Blocks
import Lipid.Format


data PA   = ClassLevelPA       IntegerMass
          | CombinedRadylsPA   CombinedRadyls
          | UnknownSnPA        Radyl Radyl
          | KnownSnPA          Radyl Radyl
          deriving (Show, Eq, Ord)


data PC   = ClassLevelPC       IntegerMass
          | CombinedRadylsPC   CombinedRadyls
          | UnknownSnPC        Radyl Radyl
          | KnownSnPC          Radyl Radyl
          deriving (Show, Eq, Ord)

data PE   = ClassLevelPE       IntegerMass
          | CombinedRadylsPE   CombinedRadyls
          | UnknownSnPE        Radyl Radyl
          | KnownSnPE          Radyl Radyl
          deriving (Show, Eq, Ord)

data PG   = ClassLevelPG       IntegerMass
          | CombinedRadylsPG   CombinedRadyls
          | UnknownSnPG        Radyl Radyl
          | KnownSnPG          Radyl Radyl
          deriving (Show, Eq, Ord)

data PGP  = ClassLevelPGP      IntegerMass
          | CombinedRadylsPGP  CombinedRadyls
          | UnknownSnPGP       Radyl Radyl
          | KnownSnPGP         Radyl Radyl
          deriving (Show, Eq, Ord)

data PI   = ClassLevelPI       IntegerMass
          | CombinedRadylsPI   CombinedRadyls
          | UnknownSnPI        Radyl Radyl
          | KnownSnPI          Radyl Radyl
          deriving (Show, Eq, Ord)

data PIP  = ClassLevelPIP      IntegerMass
          | CombinedRadylsPIP  CombinedRadyls (Maybe PhosphatePosition)
          | UnknownSnPIP       Radyl Radyl (Maybe PhosphatePosition)
          | KnownSnPIP         Radyl Radyl (Maybe PhosphatePosition)
          deriving (Show, Eq, Ord)

data PIP2 = ClassLevelPIP2     IntegerMass
          | CombinedRadylsPIP2 CombinedRadyls ( Maybe PhosphatePosition
                                              , Maybe PhosphatePosition )
          | UnknownSnPIP2      Radyl Radyl ( Maybe PhosphatePosition
                                           , Maybe PhosphatePosition )
          | KnownSnPIP2        Radyl Radyl  ( Maybe PhosphatePosition
                                            , Maybe PhosphatePosition )
          deriving (Show, Eq, Ord)

data PIP3 = ClassLevelPIP3     IntegerMass
          | CombinedRadylsPIP3 CombinedRadyls
          | UnknownSnPIP3      Radyl Radyl
          | KnownSnPIP3        Radyl Radyl
          deriving (Show, Eq, Ord)

data PS   = ClassLevelPS       IntegerMass
          | CombinedRadylsPS   CombinedRadyls
          | UnknownSnPS        Radyl Radyl
          | KnownSnPS          Radyl Radyl
          deriving (Show, Eq, Ord)

data CL   = ClassLevelCL       IntegerMass
          | CombinedRadylsCL   CombinedRadyls
          | UnknownSnCL        Radyl Radyl Radyl Radyl
          | KnownSnCL          Radyl Radyl Radyl Radyl
          deriving (Show, Eq, Ord)

data BMP  = ClassLevelBMP     IntegerMass
          | CombinedRadylsBMP CombinedRadyls
          | BMP               Radyl Radyl
          deriving (Show, Eq, Ord)


instance Shorthand PA where
    showShorthand (ClassLevelPA x)          = "PA (" ++ show x ++ ")" 
    showShorthand (CombinedRadylsPA x)      = "PA " ++ showShorthand x 
    showShorthand (UnknownSnPA x y)      = "PA " ++ fa1 ++ "_" ++ fa2
        where fa1 = showShorthand x
              fa2 = showShorthand y
    showShorthand (KnownSnPA x y)           = "PA " ++ sn1 ++ "/" ++ sn2
        where sn1 = showShorthand x
              sn2 = showShorthand y
 
instance Nomenclature PA where
    showNnomenclature (ClassLevelPA x)      = "PA (" ++ show x ++ ")" 
    showNnomenclature (CombinedRadylsPA x)  = "PA " ++ showNnomenclature x 
    showNnomenclature (UnknownSnPA x y)  = "PA " ++ fa1 ++ "_" ++ fa2
        where fa1 = showNnomenclature x
              fa2 = showNnomenclature y
    showNnomenclature (KnownSnPA x y)       = "PA " ++ sn1 ++ "/" ++ sn2
        where sn1 = showNnomenclature x
              sn2 = showNnomenclature y

instance Shorthand PC where
    showShorthand (ClassLevelPC x)          = "PC (" ++ show x ++ ")" 
    showShorthand (CombinedRadylsPC x)      = "PC " ++ showShorthand x 
    showShorthand (UnknownSnPC x y)      = "PC " ++ fa1 ++ "_" ++ fa2
        where fa1 = showShorthand x
              fa2 = showShorthand y
    showShorthand (KnownSnPC x y)           = "PC " ++ sn1 ++ "/" ++ sn2
        where sn1 = showShorthand x
              sn2 = showShorthand y
 
instance Nomenclature PC where
    showNnomenclature (ClassLevelPC x)      = "PC (" ++ show x ++ ")" 
    showNnomenclature (CombinedRadylsPC x)  = "PC " ++ showNnomenclature x 
    showNnomenclature (UnknownSnPC x y)  = "PC " ++ fa1 ++ "_" ++ fa2
        where fa1 = showNnomenclature x
              fa2 = showNnomenclature y
    showNnomenclature (KnownSnPC x y)       = "PC " ++ sn1 ++ "/" ++ sn2
        where sn1 = showNnomenclature x
              sn2 = showNnomenclature y

instance Shorthand PE where
    showShorthand (ClassLevelPE x)          = "PE (" ++ show x ++ ")" 
    showShorthand (CombinedRadylsPE x)      = "PE " ++ showShorthand x 
    showShorthand (UnknownSnPE x y)      = "PE " ++ fa1 ++ "_" ++ fa2
        where fa1 = showShorthand x
              fa2 = showShorthand y
    showShorthand (KnownSnPE x y)           = "PE " ++ sn1 ++ "/" ++ sn2
        where sn1 = showShorthand x
              sn2 = showShorthand y
 
instance Nomenclature PE where
    showNnomenclature (ClassLevelPE x)      = "PE (" ++ show x ++ ")" 
    showNnomenclature (CombinedRadylsPE x)  = "PE " ++ showNnomenclature x 
    showNnomenclature (UnknownSnPE x y)  = "PE " ++ fa1 ++ "_" ++ fa2
        where fa1 = showNnomenclature x
              fa2 = showNnomenclature y
    showNnomenclature (KnownSnPE x y)       = "PE " ++ sn1 ++ "/" ++ sn2
        where sn1 = showNnomenclature x
              sn2 = showNnomenclature y

instance Shorthand PG where
    showShorthand (ClassLevelPG x)          = "PG (" ++ show x ++ ")" 
    showShorthand (CombinedRadylsPG x)      = "PG " ++ showShorthand x 
    showShorthand (UnknownSnPG x y)      = "PG " ++ fa1 ++ "_" ++ fa2
        where fa1 = showShorthand x
              fa2 = showShorthand y
    showShorthand (KnownSnPG x y)           = "PG " ++ sn1 ++ "/" ++ sn2
        where sn1 = showShorthand x
              sn2 = showShorthand y
 
instance Nomenclature PG where
    showNnomenclature (ClassLevelPG x)      = "PG (" ++ show x ++ ")" 
    showNnomenclature (CombinedRadylsPG x)  = "PG " ++ showNnomenclature x 
    showNnomenclature (UnknownSnPG x y)  = "PG " ++ fa1 ++ "_" ++ fa2
        where fa1 = showNnomenclature x
              fa2 = showNnomenclature y
    showNnomenclature (KnownSnPG x y)       = "PG " ++ sn1 ++ "/" ++ sn2
        where sn1 = showNnomenclature x
              sn2 = showNnomenclature y

instance Shorthand PGP where
    showShorthand (ClassLevelPGP x)         = "PGP (" ++ show x ++ ")" 
    showShorthand (CombinedRadylsPGP x)     = "PGP " ++ showShorthand x 
    showShorthand (UnknownSnPGP x y)     = "PGP " ++ fa1 ++ "_" ++ fa2
        where fa1 = showShorthand x
              fa2 = showShorthand y
    showShorthand (KnownSnPGP x y)          = "PGP " ++ sn1 ++ "/" ++ sn2
        where sn1 = showShorthand x
              sn2 = showShorthand y
 
instance Nomenclature PGP where
    showNnomenclature (ClassLevelPGP x)     = "PGP (" ++ show x ++ ")" 
    showNnomenclature (CombinedRadylsPGP x) = "PGP " ++ showNnomenclature x 
    showNnomenclature (UnknownSnPGP x y) = "PGP " ++ fa1 ++ "_" ++ fa2
        where fa1 = showNnomenclature x
              fa2 = showNnomenclature y
    showNnomenclature (KnownSnPGP x y)      = "PGP " ++ sn1 ++ "/" ++ sn2
        where sn1 = showNnomenclature x
              sn2 = showNnomenclature y

instance Shorthand PI where
    showShorthand (ClassLevelPI x)         = "PI (" ++ show x ++ ")" 
    showShorthand (CombinedRadylsPI x)     = "PI " ++ showShorthand x 
    showShorthand (UnknownSnPI x y)     = "PI " ++ fa1 ++ "_" ++ fa2
        where fa1 = showShorthand x
              fa2 = showShorthand y
    showShorthand (KnownSnPI x y)          = "PI " ++ sn1 ++ "/" ++ sn2
        where sn1 = showShorthand x
              sn2 = showShorthand y
 
instance Nomenclature PI where
    showNnomenclature (ClassLevelPI x)     = "PI (" ++ show x ++ ")" 
    showNnomenclature (CombinedRadylsPI x) = "PI " ++ showNnomenclature x 
    showNnomenclature (UnknownSnPI x y) = "PI " ++ fa1 ++ "_" ++ fa2
        where fa1 = showNnomenclature x
              fa2 = showNnomenclature y
    showNnomenclature (KnownSnPI x y)      = "PI " ++ sn1 ++ "/" ++ sn2
        where sn1 = showNnomenclature x
              sn2 = showNnomenclature y

instance Shorthand PIP where
    showShorthand (ClassLevelPIP x)        = "PIP (" ++ show x ++ ")"
    showShorthand (CombinedRadylsPIP x p)  = "PIP" ++ po4 ++ " " ++ showShorthand x
        where po4 = case p of
                        Nothing  -> ""
                        (Just p') -> wrapBrackets $ showShorthand p'
    showShorthand (UnknownSnPIP x y p)  = "PIP" ++ po4 ++ " " ++ fa1 ++ "_" ++ fa2
        where fa1 = showShorthand x
              fa2 = showShorthand y
              po4 = case p of
                        Nothing  -> ""
                        (Just p') -> wrapBrackets $ showShorthand p'
    showShorthand (KnownSnPIP x y p) = "PIP" ++ po4 ++ " " ++ sn1 ++ "/" ++ sn2
        where sn1 = showShorthand x
              sn2 = showShorthand y
              po4 = case p of
                        Nothing  -> ""
                        (Just p') -> wrapBrackets $ showShorthand p'

instance Nomenclature PIP where
    showNnomenclature (ClassLevelPIP x)        = "PIP (" ++ show x ++ ")"
    showNnomenclature (CombinedRadylsPIP x p)  = "PIP" ++ po4 ++ " " ++ showNnomenclature x
        where po4 = case p of
                        Nothing  -> ""
                        (Just p') -> wrapBrackets $ showShorthand p'
    showNnomenclature (UnknownSnPIP x y p)  = "PIP" ++ po4 ++ " " ++ fa1 ++ "_" ++ fa2
        where fa1 = showNnomenclature x
              fa2 = showNnomenclature y
              po4 = case p of
                        Nothing  -> ""
                        (Just p') -> wrapBrackets $ showShorthand p'
    showNnomenclature (KnownSnPIP x y p)       = "PIP" ++ po4 ++ " " ++ sn1 ++ "/" ++ sn2
        where sn1 = showNnomenclature x
              sn2 = showNnomenclature y
              po4 = case p of
                        Nothing  -> ""
                        (Just p') -> wrapBrackets $ showShorthand p'

instance Shorthand PIP2 where
    showShorthand (ClassLevelPIP2 x)          = "PIP2 (" ++ show x ++ ")"
    showShorthand (CombinedRadylsPIP2 x p)    = "PIP2" ++ po4 ++ " " ++ showShorthand x
        where po4 = case p of
                        (Nothing, _)  -> ""
                        (_, Nothing)  -> ""
                        (Just p1, Just p2) -> wrapBrackets $ showShorthand p1 ++ "," ++ showShorthand p2
    showShorthand (UnknownSnPIP2 x y p)    = "PIP2" ++ po4 ++ " " ++ fa1 ++ "_" ++ fa2
        where fa1 = showShorthand x
              fa2 = showShorthand y
              po4 = case p of
                        (Nothing, _)  -> ""
                        (_, Nothing)  -> ""
                        (Just p1, Just p2) -> wrapBrackets $ showShorthand p1 ++ "," ++ showShorthand p2
    showShorthand (KnownSnPIP2 x y p) = "PIP2" ++ po4 ++ " " ++ sn1 ++ "/" ++ sn2
        where sn1 = showShorthand x
              sn2 = showShorthand y
              po4 = case p of
                        (Nothing, _)  -> ""
                        (_, Nothing)  -> ""
                        (Just p1, Just p2) -> wrapBrackets $ showShorthand p1 ++ "," ++ showShorthand p2

instance Nomenclature PIP2 where
    showNnomenclature (ClassLevelPIP2 x)        = "PIP (" ++ show x ++ ")"
    showNnomenclature (CombinedRadylsPIP2 x p)  = "PIP" ++ po4 ++ " " ++ showNnomenclature x
        where po4 = case p of
                        (Nothing, _)  -> ""
                        (_, Nothing)  -> ""
                        (Just p1, Just p2) -> wrapBrackets $ showShorthand p1 ++ "," ++ showShorthand p2
    showNnomenclature (UnknownSnPIP2 x y p)  = "PIP" ++ po4 ++ " " ++ fa1 ++ "_" ++ fa2
        where fa1 = showNnomenclature x
              fa2 = showNnomenclature y
              po4 = case p of
                        (Nothing, _)  -> ""
                        (_, Nothing)  -> ""
                        (Just p1, Just p2) -> wrapBrackets $ showShorthand p1 ++ "," ++ showShorthand p2
    showNnomenclature (KnownSnPIP2 x y p)       = "PIP" ++ po4 ++ " " ++ sn1 ++ "/" ++ sn2
        where sn1 = showNnomenclature x
              sn2 = showNnomenclature y
              po4 = case p of
                        (Nothing, _)  -> ""
                        (_, Nothing)  -> ""
                        (Just p1, Just p2) -> wrapBrackets $ showShorthand p1 ++ "," ++ showShorthand p2

instance Shorthand PIP3 where
    showShorthand (ClassLevelPIP3 x)         = "PIP3 (" ++ show x ++ ")" 
    showShorthand (CombinedRadylsPIP3 x)     = "PIP3 " ++ showShorthand x 
    showShorthand (UnknownSnPIP3 x y)     = "PIP3 " ++ fa1 ++ "_" ++ fa2
        where fa1 = showShorthand x
              fa2 = showShorthand y
    showShorthand (KnownSnPIP3 x y)          = "PIP3 " ++ sn1 ++ "/" ++ sn2
        where sn1 = showShorthand x
              sn2 = showShorthand y
 
instance Nomenclature PIP3 where
    showNnomenclature (ClassLevelPIP3 x)     = "PIP3 (" ++ show x ++ ")" 
    showNnomenclature (CombinedRadylsPIP3 x) = "PIP3 " ++ showNnomenclature x 
    showNnomenclature (UnknownSnPIP3 x y) = "PIP3 " ++ fa1 ++ "_" ++ fa2
        where fa1 = showNnomenclature x
              fa2 = showNnomenclature y
    showNnomenclature (KnownSnPIP3 x y)      = "PIP3 " ++ sn1 ++ "/" ++ sn2
        where sn1 = showNnomenclature x
              sn2 = showNnomenclature y

instance Shorthand PS where
    showShorthand (ClassLevelPS x)           = "PS (" ++ show x ++ ")" 
    showShorthand (CombinedRadylsPS x)       = "PS " ++ showShorthand x 
    showShorthand (UnknownSnPS x y)       = "PS " ++ fa1 ++ "_" ++ fa2
        where fa1 = showShorthand x
              fa2 = showShorthand y
    showShorthand (KnownSnPS x y)            = "PS " ++ sn1 ++ "/" ++ sn2
        where sn1 = showShorthand x
              sn2 = showShorthand y
 
instance Nomenclature PS where
    showNnomenclature (ClassLevelPS x)       = "PS (" ++ show x ++ ")" 
    showNnomenclature (CombinedRadylsPS x)   = "PS " ++ showNnomenclature x 
    showNnomenclature (UnknownSnPS x y)   = "PS " ++ fa1 ++ "_" ++ fa2
        where fa1 = showNnomenclature x
              fa2 = showNnomenclature y
    showNnomenclature (KnownSnPS x y)        = "PS " ++ sn1 ++ "/" ++ sn2
        where sn1 = showNnomenclature x
              sn2 = showNnomenclature y

instance Shorthand CL where
    showShorthand (ClassLevelCL x)           = "CL (" ++ show x ++ ")" 
    showShorthand (CombinedRadylsCL x)       = "CL " ++ showShorthand x 
    showShorthand (UnknownSnCL x y z w) = "CL " ++ fa1 ++ "_" ++ fa2 ++ "_" ++ fa3 ++ "_" ++ fa4
        where fa1 = showShorthand x
              fa2 = showShorthand y
              fa3 = showShorthand z
              fa4 = showShorthand w
    showShorthand (KnownSnCL x y z w)        = "CL " ++ sn1 ++ "/" ++ sn2 ++ "/" ++ sn1' ++ "/" ++ sn2'
        where sn1 = showShorthand x
              sn2 = showShorthand y
              sn1' = showShorthand z
              sn2' = showShorthand w
 
instance Nomenclature CL where
    showNnomenclature (ClassLevelCL x)           = "CL (" ++ show x ++ ")" 
    showNnomenclature (CombinedRadylsCL x)       = "CL " ++ showNnomenclature x 
    showNnomenclature (UnknownSnCL x y z w) = "CL " ++ fa1 ++ "_" ++ fa2 ++ "_" ++ fa3 ++ "_" ++ fa4
        where fa1 = showNnomenclature x
              fa2 = showNnomenclature y
              fa3 = showNnomenclature z
              fa4 = showNnomenclature w
    showNnomenclature (KnownSnCL x y z w)        = "CL " ++ sn1 ++ "/" ++ sn2 ++ "/" ++ sn1' ++ "/" ++ sn2'
        where sn1 = showNnomenclature x
              sn2 = showNnomenclature y
              sn1' = showNnomenclature z
              sn2' = showNnomenclature w

instance Shorthand BMP where
    showShorthand (ClassLevelBMP x)           = "BMP (" ++ show x ++ ")" 
    showShorthand (CombinedRadylsBMP x)       = "BMP " ++ showShorthand x 
    showShorthand (BMP x y)       = "BMP " ++ fa1 ++ "/" ++ fa2
        where fa1 = showShorthand x
              fa2 = showShorthand y
 
instance Nomenclature BMP where
    showNnomenclature (ClassLevelBMP x)       = "BMP (" ++ show x ++ ")" 
    showNnomenclature (CombinedRadylsBMP x)   = "BMP " ++ showNnomenclature x 
    showNnomenclature (BMP x y)   = "BMP " ++ fa1 ++ "/" ++ fa2
        where fa1 = showNnomenclature x
              fa2 = showNnomenclature y

