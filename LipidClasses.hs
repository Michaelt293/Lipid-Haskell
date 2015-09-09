{-|
Module      : LipidClasses
Description :
Copyright   : Michael Thomas 
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : experimental
-}

module LipidClasses where

import ElementIsotopes
import BuildingBlocks

type PhosphatePosition = Integer

data FA   = ClassLevelFA       IntegerMass
          | FA                 CarbonChain
          deriving (Show, Eq, Ord)

data MG   = ClassLevelMG       IntegerMass
          | UnknownSn          Radyl
          | Sn1MG              Radyl
          | Sn2MG              Radyl
          | Sn3MG              Radyl
          deriving (Show, Eq, Ord)

data DG   = ClassLevelDG       IntegerMass
          | CombinedRadylsDG   CombinedRadyls
          | UnknownDG          (Radyl, Radyl)
          | Sn12DG             { dgSn1 :: Radyl
                               , dgSn2 :: Radyl } 
          | Sn13DG             { dgSn1 :: Radyl
                               , dgSn3 :: Radyl }
          | Sn23DG             { dgSn2 :: Radyl
                               , dgSn3 :: Radyl }
          deriving (Show, Eq, Ord)

data TG   = ClassLevelTG       IntegerMass
          | CombinedRadylsTG   CombinedRadyls
          | UnknownSnTG        (Radyl, Radyl, Radyl)
          | KnownSnTG          { tgSn1 :: Radyl
                               , tgSn2 :: Radyl
                               , tgSn3 :: Radyl }
          deriving (Show, Eq, Ord)

data PA   = ClassLevelPA       IntegerMass
          | CombinedRadylsPA   CombinedRadyls
          | UnknownSnPA        (Radyl, Radyl)
          | KnownSnPA          { paSn1 :: Radyl
                               , paSn2 :: Radyl }
          | UnknownSnLPA       Radyl
          | Sn1LPA             Radyl
          | Sn2LPA             Radyl
          deriving (Show, Eq, Ord)


data PC   = ClassLevelPC       IntegerMass
          | CombinedRadylsPC   CombinedRadyls
          | UnknownSnPC        (Radyl, Radyl)
          | KnownSnPC          { pcSn1 :: Radyl
                               , pcSn2 :: Radyl }
          | UnknownSnLPC       Radyl
          | Sn1LPC             Radyl
          | Sn2LPC             Radyl
          deriving (Show, Eq, Ord)

data PE   = ClassLevelPE       IntegerMass
          | CombinedRadylPE    CombinedRadyls
          | UnknownSnPE        (Radyl, Radyl)
          | KnownSnPE          { peSn1 :: Radyl
                               , peSn2 :: Radyl }
          | UnknownSnLPE       Radyl
          | Sn1LPE             Radyl
          | Sn2LPE             Radyl
          deriving (Show, Eq, Ord)

data PG   = ClassLevelPG       IntegerMass
          | CombinedRadylsPG   CombinedRadyls
          | UnknownSnPG        (Radyl, Radyl)
          | KnownSnPG          { pgSn1 :: Radyl 
                               , pgSn2 :: Radyl }
          | UnknownSnLPG       Radyl
          | Sn1LPG             Radyl
          | Sn2LPG             Radyl
          deriving (Show, Eq, Ord)

data PGP  = ClassLevelPGP      IntegerMass
          | CombinedRadylsPGP  CombinedRadyls
          | UnknownSnPGP       (Radyl, Radyl)
          | KnownSnPGP         { pgpSn1 :: Radyl
                               , pgpSn2 :: Radyl }
          | UnknownSnLPGP      Radyl
          | Sn1LPGP            Radyl
          | Sn2LPGP            Radyl
          deriving (Show, Eq, Ord)

data PI   = ClassLevelPI       IntegerMass
          | CombinedRadylsPI   CombinedRadyls
          | UnknownSnPI        (Radyl, Radyl)
          | KnownSnPI          { piSn1 :: Radyl
                               , piSn2 :: Radyl }
          | UnknownSnLPI       Radyl
          | Sn1LPI             Radyl
          | Sn2LPI             Radyl
          deriving (Show, Eq, Ord)

data PIP  = ClassLevelPIP      IntegerMass
          | CombinedRadylsPIP  CombinedRadyls (Maybe PhosphatePosition)
          | UnknownSnPIP       (Radyl, Radyl) (Maybe PhosphatePosition)
          | KnownSnPIP         { pipSn1 :: Radyl
                               , pipSn2 :: Radyl
                               , pipPO4 :: (Maybe PhosphatePosition) }
          | UnknownSnLPIP      Radyl (Maybe PhosphatePosition)
          | Sn1LPIP            Radyl (Maybe PhosphatePosition)
          | Sn2LPIP            Radyl (Maybe PhosphatePosition)
          deriving (Show, Eq, Ord)

data PIP2 = ClassLevelPIP2     IntegerMass
          | CombinedRadylsPIP2 CombinedRadyls (Maybe PhosphatePosition, Maybe PhosphatePosition)
          | UnknownSnPIP2      (Radyl, Radyl)
                               (Maybe PhosphatePosition, Maybe PhosphatePosition)
          | KnownSnPIP2        { pip2Sn1 :: Radyl
                               , pip2Sn2 :: Radyl
                               , pip2PO4 :: (Maybe PhosphatePosition, Maybe PhosphatePosition)}
          | UnknownSnLPIP2     Radyl (Maybe PhosphatePosition, Maybe PhosphatePosition)
          | Sn1LPIP2           Radyl (Maybe PhosphatePosition, Maybe PhosphatePosition)
          | Sn2LPIP2           Radyl (Maybe PhosphatePosition, Maybe PhosphatePosition)
          deriving (Show, Eq, Ord)

data PIP3 = ClassLevelPIP3     IntegerMass
          | CombinedRadylsPIP3 CombinedRadyls
          | UnknownSnPIP3      (Radyl, Radyl)
          | KnownSnPIP3        { pip3Sn1 :: Radyl
                               , pip3Sn2 :: Radyl }
          | UnknownSnLPIP3     Radyl
          | Sn1LPIP3           Radyl
          | Sn2LPIP3           Radyl
          deriving (Show, Eq, Ord)

data PS   = ClassLevelPS       IntegerMass
          | CombinedRadylsPS   CombinedRadyls
          | UnknownSnPS        (Radyl, Radyl)
          | KnownSnPS          { psSn1 :: Radyl
                               , psSn2 :: Radyl }
          | UnknownSnLPS       Radyl
          | Sn1LPS             Radyl
          | Sn2LPS             Radyl
          deriving (Show, Eq, Ord)

data CL   = ClassLevelCL       IntegerMass
          | CombinedRadylsCL   CombinedRadyls
          | UnknownSnCL        (Radyl, Radyl, Radyl, Radyl)
          | KnownSnCL          { clSn1 :: Radyl
                               , clSn2 :: Radyl
                               , clSn3 :: Radyl
                               , clSn4 :: Radyl }
          | UnknownSnLCL        [Radyl]
          deriving (Show, Eq, Ord)


instance Shorthand FA where
    showShorthand (ClassLevelFA x) = "FA (" ++ show x ++ ")"
    showShorthand (FA x) = "FA " ++ showShorthand x

instance Nomenclature FA where
    showNnomenclature (ClassLevelFA x) = "FA (" ++ show x ++ ")"
    showNnomenclature (FA x) = "FA " ++ showNnomenclature x

instance Shorthand DG where
    showShorthand (ClassLevelDG x) = "DG (" ++ show x ++ ")"
    