{-|
Module      : Lipid.Glycerophospholipid
Description : Glycerophospholipid data types and instances of Shorthand and
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
          | CombinedRadylsPA   TwoCombinedRadyls
          | UnknownSnPA        Radyl Radyl
          | KnownSnPA          Radyl Radyl
          deriving (Show, Eq, Ord)

data PC   = ClassLevelPC       IntegerMass
          | CombinedRadylsPC   TwoCombinedRadyls
          | UnknownSnPC        Radyl Radyl
          | KnownSnPC          Radyl Radyl
          deriving (Show, Eq, Ord)

data PE   = ClassLevelPE       IntegerMass
          | CombinedRadylsPE   TwoCombinedRadyls
          | UnknownSnPE        Radyl Radyl
          | KnownSnPE          Radyl Radyl
          deriving (Show, Eq, Ord)

data PG   = ClassLevelPG       IntegerMass
          | CombinedRadylsPG   TwoCombinedRadyls
          | UnknownSnPG        Radyl Radyl
          | KnownSnPG          Radyl Radyl
          deriving (Show, Eq, Ord)

data PGP  = ClassLevelPGP      IntegerMass
          | CombinedRadylsPGP  TwoCombinedRadyls
          | UnknownSnPGP       Radyl Radyl
          | KnownSnPGP         Radyl Radyl
          deriving (Show, Eq, Ord)

data PI   = ClassLevelPI       IntegerMass
          | CombinedRadylsPI   TwoCombinedRadyls
          | UnknownSnPI        Radyl Radyl
          | KnownSnPI          Radyl Radyl
          deriving (Show, Eq, Ord)

data PIP  = ClassLevelPIP      IntegerMass
          | CombinedRadylsPIP  TwoCombinedRadyls (Maybe PhosphatePosition)
          | UnknownSnPIP       Radyl Radyl (Maybe PhosphatePosition)
          | KnownSnPIP         Radyl Radyl (Maybe PhosphatePosition)
          deriving (Show, Eq, Ord)

data PIP2 = ClassLevelPIP2     IntegerMass
          | CombinedRadylsPIP2 TwoCombinedRadyls ( Maybe PhosphatePosition
                                              , Maybe PhosphatePosition )
          | UnknownSnPIP2      Radyl Radyl ( Maybe PhosphatePosition
                                           , Maybe PhosphatePosition )
          | KnownSnPIP2        Radyl Radyl  ( Maybe PhosphatePosition
                                            , Maybe PhosphatePosition )
          deriving (Show, Eq, Ord)

data PIP3 = ClassLevelPIP3     IntegerMass
          | CombinedRadylsPIP3 TwoCombinedRadyls
          | UnknownSnPIP3      Radyl Radyl
          | KnownSnPIP3        Radyl Radyl
          deriving (Show, Eq, Ord)

data PS   = ClassLevelPS       IntegerMass
          | CombinedRadylsPS   TwoCombinedRadyls
          | UnknownSnPS        Radyl Radyl
          | KnownSnPS          Radyl Radyl
          deriving (Show, Eq, Ord)

data CL   = ClassLevelCL       IntegerMass
          | CombinedRadylsCL   FourCombinedRadyls
          | UnknownSnCL        Radyl Radyl Radyl Radyl
          | KnownSnCL          Radyl Radyl Radyl Radyl
          deriving (Show, Eq, Ord)

data BMP  = ClassLevelBMP     IntegerMass
          | CombinedRadylsBMP TwoCombinedRadyls
          | BMP               Radyl Radyl
          deriving (Show, Eq, Ord)


instance Shorthand PA where
    showShorthand l =
      case l of
        (ClassLevelPA n)      -> "PA (" ++ show n ++ ")"
        (CombinedRadylsPA rs) -> "PA " ++ showShorthand rs
        (UnknownSnPA r1 r2)   -> renderDiradylPL showShorthand "PA" "_" r1 r2
        (KnownSnPA r1 r2)     -> renderDiradylPL showShorthand "PA" "/" r1 r2
 
instance Nomenclature PA where
    showNnomenclature l =
      case l of
        (ClassLevelPA n)      -> "PA (" ++ show n ++ ")"
        (CombinedRadylsPA rs) -> "PA " ++ showNnomenclature rs
        (UnknownSnPA r1 r2)   -> renderDiradylPL showNnomenclature "PA" "_" r1 r2
        (KnownSnPA r1 r2)     -> renderDiradylPL showNnomenclature "PA" "/" r1 r2

instance Shorthand PC where
    showShorthand l =
      case l of
        (ClassLevelPC n)      -> "PC (" ++ show n ++ ")"
        (CombinedRadylsPC rs) -> "PC " ++ showShorthand rs
        (UnknownSnPC r1 r2)   -> renderDiradylPL showShorthand "PC" "_" r1 r2
        (KnownSnPC r1 r2)     -> renderDiradylPL showShorthand "PC" "/" r1 r2
 
instance Nomenclature PC where
    showNnomenclature l =
      case l of
        (ClassLevelPC n)      -> "PC (" ++ show n ++ ")"
        (CombinedRadylsPC rs) -> "PC " ++ showNnomenclature rs
        (UnknownSnPC r1 r2)   -> renderDiradylPL showNnomenclature "PC" "_" r1 r2
        (KnownSnPC r1 r2)     -> renderDiradylPL showNnomenclature "PC" "/" r1 r2

instance Shorthand PE where
    showShorthand l =
      case l of
        (ClassLevelPE n)      -> "PE (" ++ show n ++ ")"
        (CombinedRadylsPE rs) -> "PE " ++ showShorthand rs
        (UnknownSnPE r1 r2)   -> renderDiradylPL showShorthand "PE" "_" r1 r2
        (KnownSnPE r1 r2)     -> renderDiradylPL showShorthand "PE" "/" r1 r2
 
instance Nomenclature PE where
    showNnomenclature l =
      case l of
        (ClassLevelPE n)      -> "PE (" ++ show n ++ ")"
        (CombinedRadylsPE rs) -> "PE " ++ showNnomenclature rs
        (UnknownSnPE r1 r2)   -> renderDiradylPL showNnomenclature "PE" "_" r1 r2
        (KnownSnPE r1 r2)     -> renderDiradylPL showNnomenclature "PE" "/" r1 r2

instance Shorthand PG where
    showShorthand l =
      case l of
        (ClassLevelPG n)      -> "PG (" ++ show n ++ ")"
        (CombinedRadylsPG rs) -> "PG " ++ showShorthand rs
        (UnknownSnPG r1 r2)   -> renderDiradylPL showShorthand "PG" "_" r1 r2
        (KnownSnPG r1 r2)     -> renderDiradylPL showShorthand "PG" "/" r1 r2
 
instance Nomenclature PG where
    showNnomenclature l =
      case l of
        (ClassLevelPG n)      -> "PG (" ++ show n ++ ")"
        (CombinedRadylsPG rs) -> "PG " ++ showNnomenclature rs
        (UnknownSnPG r1 r2)   -> renderDiradylPL showNnomenclature "PG" "_" r1 r2
        (KnownSnPG r1 r2)     -> renderDiradylPL showNnomenclature "PG" "/" r1 r2

instance Shorthand PGP where
    showShorthand l =
      case l of
        (ClassLevelPGP n)      -> "PGP (" ++ show n ++ ")"
        (CombinedRadylsPGP rs) -> "PGP " ++ showShorthand rs
        (UnknownSnPGP r1 r2)   -> renderDiradylPL showShorthand "PGP" "_" r1 r2
        (KnownSnPGP r1 r2)     -> renderDiradylPL showShorthand "PGP" "/" r1 r2

instance Nomenclature PGP where
    showNnomenclature l =
      case l of
        (ClassLevelPGP n)      -> "PGP (" ++ show n ++ ")"
        (CombinedRadylsPGP rs) -> "PGP " ++ showNnomenclature rs
        (UnknownSnPGP r1 r2)   -> renderDiradylPL showNnomenclature "PGP" "_" r1 r2
        (KnownSnPGP r1 r2)     -> renderDiradylPL showNnomenclature "PGP" "/" r1 r2

instance Shorthand PI where
    showShorthand l =
      case l of
        (ClassLevelPI n)      -> "PI (" ++ show n ++ ")"
        (CombinedRadylsPI rs) -> "PI " ++ showShorthand rs
        (UnknownSnPI r1 r2)   -> renderDiradylPL showShorthand "PI" "_" r1 r2
        (KnownSnPI r1 r2)     -> renderDiradylPL showShorthand "PI" "/" r1 r2

instance Nomenclature PI where
    showNnomenclature l =
      case l of
        (ClassLevelPI n)      -> "PI (" ++ show n ++ ")"
        (CombinedRadylsPI rs) -> "PI " ++ showNnomenclature rs
        (UnknownSnPI r1 r2)   -> renderDiradylPL showNnomenclature "PI" "_" r1 r2
        (KnownSnPI r1 r2)     -> renderDiradylPL showNnomenclature "PI" "/" r1 r2

instance Shorthand PIP where
    showShorthand l =
      case l of
        (ClassLevelPIP n)       ->   "PIP (" ++ show n ++ ")"
        (CombinedRadylsPIP rs p) ->   "PIP" ++ renderPO4 p ++ " " ++ showShorthand rs
        (UnknownSnPIP r1 r2 p)   -> renderPIPs showShorthand renderPO4 "_" r1 r2 p
        (KnownSnPIP r1 r2 p)     -> renderPIPs showShorthand renderPO4 "/" r1 r2 p

instance Nomenclature PIP where
    showNnomenclature l =
      case l of
        (ClassLevelPIP n)       ->   "PIP (" ++ show n ++ ")"
        (CombinedRadylsPIP rs p) ->   "PIP" ++ renderPO4 p ++ " " ++ showNnomenclature rs
        (UnknownSnPIP r1 r2 p)   -> renderPIPs showNnomenclature renderPO4 "_" r1 r2 p
        (KnownSnPIP r1 r2 p)     -> renderPIPs showNnomenclature renderPO4 "/" r1 r2 p

instance Shorthand PIP2 where
    showShorthand l =
      case l of
        (ClassLevelPIP2 n)         -> "PIP2 (" ++ show n ++ ")"
        (CombinedRadylsPIP2 rs ps) -> "PIP2" ++ renderPO4s ps ++ " " ++ showShorthand rs
        (UnknownSnPIP2 r1 r2 ps)   -> renderPIPs showShorthand renderPO4s "_" r1 r2 ps
        (KnownSnPIP2 r1 r2 ps)     -> renderPIPs showShorthand renderPO4s "/" r1 r2 ps

instance Nomenclature PIP2 where
    showNnomenclature l =
      case l of
        (ClassLevelPIP2 n)         -> "PIP (" ++ show n ++ ")"
        (CombinedRadylsPIP2 rs ps) -> "PIP" ++ renderPO4s ps ++ " " ++ showNnomenclature rs
        (UnknownSnPIP2 r1 r2 ps)   -> renderPIPs showNnomenclature renderPO4s "_" r1 r2 ps
        (KnownSnPIP2 r1 r2 ps)     -> renderPIPs showNnomenclature renderPO4s "/" r1 r2 ps

renderPIPs f1 f2 sep r1 r2 p  = "PIP" ++ f2 p ++ " " ++ r1' ++ "_" ++ r2'
    where r1' = f1 r1
          r2' = f1 r2

renderPO4 p =
  case p of
    Nothing  -> ""
    (Just p') -> wrapBrackets $ showShorthand p'

renderPO4s ps =
  case ps of
    (Just p1, Just p2) -> wrapBrackets $ showShorthand p1 ++ "," ++ showShorthand p2
    (_, _)  -> ""

instance Shorthand PIP3 where
    showShorthand l =
      case l of
        (ClassLevelPIP3 n)      -> "PIP3 (" ++ show n ++ ")"
        (CombinedRadylsPIP3 rs) -> "PIP3 " ++ showShorthand rs
        (UnknownSnPIP3 r1 r2)   -> renderDiradylPL showShorthand "PS" "_" r1 r2
        (KnownSnPIP3 r1 r2)     -> renderDiradylPL showShorthand "PS" "_" r1 r2

instance Nomenclature PIP3 where
    showNnomenclature l =
      case l of
        (ClassLevelPIP3 n)      -> "PIP3 (" ++ show n ++ ")"
        (CombinedRadylsPIP3 rs) -> "PIP3 " ++ showNnomenclature rs
        (UnknownSnPIP3 r1 r2)   -> renderDiradylPL showNnomenclature "PS" "_" r1 r2
        (KnownSnPIP3 r1 r2)     -> renderDiradylPL showNnomenclature "PS" "_" r1 r2

instance Shorthand PS where
    showShorthand l =
      case l of
        (ClassLevelPS n)      -> "PS (" ++ show n ++ ")"
        (CombinedRadylsPS rs) -> "PS " ++ showShorthand rs
        (UnknownSnPS r1 r2)   -> renderDiradylPL showShorthand "PS" "_" r1 r2
        (KnownSnPS r1 r2)     -> renderDiradylPL showShorthand "PS" "/" r1 r2
 
instance Nomenclature PS where
    showNnomenclature l =
      case l of
        (ClassLevelPS n)      -> "PS (" ++ show n ++ ")"
        (CombinedRadylsPS rs) -> "PS " ++ showNnomenclature rs
        (UnknownSnPS r1 r2)   -> renderDiradylPL showNnomenclature "PS" "_" r1 r2
        (KnownSnPS r1 r2)     -> renderDiradylPL showNnomenclature "PS" "/" r1 r2

instance Shorthand CL where
    showShorthand l =
      case l of
        (ClassLevelCL n)          -> "CL (" ++ show n ++ ")"
        (CombinedRadylsCL rs)     -> "CL " ++ showShorthand rs
        (UnknownSnCL r1 r2 r3 r4) -> renderCL showShorthand "_" r1 r2 r3 r4
        (KnownSnCL r1 r2 r3 r4)   -> renderCL showShorthand "/" r1 r2 r3 r4

instance Nomenclature CL where
    showNnomenclature l =
      case l of
        (ClassLevelCL n)          -> "CL (" ++ show n ++ ")"
        (CombinedRadylsCL rs)     -> "CL " ++ showNnomenclature rs
        (UnknownSnCL r1 r2 r3 r4) -> renderCL showNnomenclature "_" r1 r2 r3 r4
        (KnownSnCL r1 r2 r3 r4)   -> renderCL showNnomenclature "/" r1 r2 r3 r4

renderCL f sep r1 r2 r3 r4 = "CL " ++ r1' ++ sep ++ r2' ++ sep ++ r3'++ sep ++ r4'
    where r1' = f r1
          r2' = f r2
          r3' = f r3
          r4' = f r4

instance Shorthand BMP where
    showShorthand l =
      case l of
        (ClassLevelBMP n)      -> "BMP (" ++ show n ++ ")"
        (CombinedRadylsBMP rs) -> "BMP " ++ showShorthand rs
        (BMP r1 r2)            -> renderDiradylPL showShorthand "BMP" "/" r1 r2

instance Nomenclature BMP where
    showNnomenclature l =
      case l of
        (ClassLevelBMP n)      -> "BMP (" ++ show n ++ ")"
        (CombinedRadylsBMP rs) -> "BMP " ++ showNnomenclature rs
        (BMP r1 r2)            -> renderDiradylPL showNnomenclature "BMP" "/" r1 r2

renderDiradylPL f h sep r1 r2 = h ++ " " ++ r1' ++ sep ++ r2'
    where r1' = f r1
          r2' = f r2
