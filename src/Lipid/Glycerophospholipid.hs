{-|
Module      : Lipid.Glycerophospholipid
Description : Glycerophospholipid data types and instances of Shorthand and
              NNomenclature defined.
Copyright   : Michael Thomas
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TemplateHaskell #-}

module Lipid.Glycerophospholipid where

import Lipid.Blocks
import Lipid.Format
import Lipid.ClassLevel
import qualified Lipid.KnownSn.Glycerophospholipid as KnownSn.Glycerophospholipid
import Control.Lens
import Data.Monoid ((<>))


data PA a
  = ClassLevelPA       ClassLevel
  | CombinedRadylsPA   (TwoCombinedRadyls a)
  | UnknownSnPA        (KnownSn.Glycerophospholipid.PA a) -- fix
  | KnownSnPA          (KnownSn.Glycerophospholipid.PA a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makePrisms ''PA

data PC a
  = ClassLevelPC       ClassLevel
  | CombinedRadylsPC   (TwoCombinedRadyls a)
  | UnknownSnPC        (Radyl a) (Radyl a)
  | KnownSnPC          (KnownSn.Glycerophospholipid.PC a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makePrisms ''PC

data PE a
  = ClassLevelPE       ClassLevel
  | CombinedRadylsPE   (TwoCombinedRadyls a)
  | UnknownSnPE        (Radyl a) (Radyl a)
  | KnownSnPE          (KnownSn.Glycerophospholipid.PE a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makePrisms ''ClassLevel

data PG a
  = ClassLevelPG       ClassLevel
  | CombinedRadylsPG   (TwoCombinedRadyls a)
  | UnknownSnPG        (Radyl a) (Radyl a)
  | KnownSnPG          (KnownSn.Glycerophospholipid.PG a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makePrisms ''PG

data PGP a
  = ClassLevelPGP      ClassLevel
  | CombinedRadylsPGP  (TwoCombinedRadyls a)
  | UnknownSnPGP       (Radyl a) (Radyl a)
  | KnownSnPGP         (KnownSn.Glycerophospholipid.PGP a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makePrisms ''PGP

data PI a
  = ClassLevelPI       ClassLevel
  | CombinedRadylsPI   (TwoCombinedRadyls a)
  | UnknownSnPI        (Radyl a) (Radyl a)
  | KnownSnPI          (KnownSn.Glycerophospholipid.PI a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makePrisms ''PI

data PIP a
  = ClassLevelPIP      ClassLevel
  | CombinedRadylsPIP  (TwoCombinedRadyls a)
  | UnknownSnPIP       (Radyl a) (Radyl a)
  | KnownSnPIP         (KnownSn.Glycerophospholipid.PIP a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makePrisms ''PIP

data PIP2 a
  = ClassLevelPIP2     ClassLevel
  | CombinedRadylsPIP2 (TwoCombinedRadyls a)
  | UnknownSnPIP2      (Radyl a) (Radyl a)
  | KnownSnPIP2        (KnownSn.Glycerophospholipid.PIP2 a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makePrisms ''PIP2

data PIP3 a
  = ClassLevelPIP3     ClassLevel
  | CombinedRadylsPIP3 (TwoCombinedRadyls a)
  | UnknownSnPIP3      (Radyl a) (Radyl a)
  | KnownSnPIP3        (KnownSn.Glycerophospholipid.PIP3 a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makePrisms ''PIP3

data PS a
  = ClassLevelPS       ClassLevel
  | CombinedRadylsPS   (TwoCombinedRadyls a)
  | UnknownSnPS        (Radyl a) (Radyl a)
  | KnownSnPS          (KnownSn.Glycerophospholipid.PS a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makePrisms ''PS
-- data CL   = ClassLevelCL       Integer
--           | CombinedRadylsCL   FourCombinedRadyls
--           | UnknownSnCL        Radyl Radyl Radyl Radyl
--           | KnownSnCL          Radyl Radyl Radyl Radyl
          -- deriving (Show, Eq, Ord)

-- data BMP a
--   = ClassLevelBMP     Integer
--   | CombinedRadylsBMP (TwoCombinedRadyls a)
--   | BMP               (Glycerol PhosphatidicAcid (Radyl a) (Radyl a))
--   deriving (Show, Eq, Ord)


instance Shorthand a => Shorthand (PA a) where
  shorthand =
    \case
      ClassLevelPA n      -> "PA (" <> show n <> ")"
      CombinedRadylsPA rs -> "PA " <> shorthand rs
      UnknownSnPA r1 r2   -> renderDiradylPL shorthand "PA" "_" r1 r2
      KnownSnPA g         -> shorthand g

instance NNomenclature a => NNomenclature (PA a) where
  nNomenclature =
    \case
      ClassLevelPA n      -> "PA (" <> show n <> ")"
      CombinedRadylsPA rs -> "PA " <> nNomenclature rs
      UnknownSnPA r1 r2   -> renderDiradylPL nNomenclature "PA" "_" r1 r2
      KnownSnPA g         -> nNomenclature g

instance Shorthand a => Shorthand (PC a) where
  shorthand =
    \case
      ClassLevelPC n      -> "PC (" <> show n <> ")"
      CombinedRadylsPC rs -> "PC " <> shorthand rs
      UnknownSnPC r1 r2   -> renderDiradylPL shorthand "PC" "_" r1 r2
      KnownSnPC g         -> shorthand g

instance NNomenclature a => NNomenclature (PC a) where
  nNomenclature =
    \case
      ClassLevelPC n      -> "PC (" <> show n <> ")"
      CombinedRadylsPC rs -> "PC " <> nNomenclature rs
      UnknownSnPC r1 r2   -> renderDiradylPL nNomenclature "PC" "_" r1 r2
      KnownSnPC g         -> nNomenclature g

instance Shorthand a => Shorthand (PE a) where
  shorthand =
    \case
      ClassLevelPE n      -> "PE (" <> show n <> ")"
      CombinedRadylsPE rs -> "PE " <> shorthand rs
      UnknownSnPE r1 r2   -> renderDiradylPL shorthand "PE" "_" r1 r2
      KnownSnPE g         -> shorthand g

instance NNomenclature a => NNomenclature (PE a) where
  nNomenclature =
    \case
      ClassLevelPE n      -> "PE (" <> show n <> ")"
      CombinedRadylsPE rs -> "PE " <> nNomenclature rs
      UnknownSnPE r1 r2   -> renderDiradylPL nNomenclature "PE" "_" r1 r2
      KnownSnPE g         -> nNomenclature g

instance Shorthand a => Shorthand (PG a) where
  shorthand =
    \case
      ClassLevelPG n      -> "PG (" <> show n <> ")"
      CombinedRadylsPG rs -> "PG " <> shorthand rs
      UnknownSnPG r1 r2   -> renderDiradylPL shorthand "PG" "_" r1 r2
      KnownSnPG g         -> shorthand g

instance NNomenclature a => NNomenclature (PG a) where
  nNomenclature =
    \case
      ClassLevelPG n      -> "PG (" <> show n <> ")"
      CombinedRadylsPG rs -> "PG " <> nNomenclature rs
      UnknownSnPG r1 r2   -> renderDiradylPL nNomenclature "PG" "_" r1 r2
      KnownSnPG g         -> nNomenclature g

instance Shorthand a => Shorthand (PGP a) where
  shorthand =
    \case
      ClassLevelPGP n      -> "PGP (" <> show n <> ")"
      CombinedRadylsPGP rs -> "PGP " <> shorthand rs
      UnknownSnPGP r1 r2   -> renderDiradylPL shorthand "PGP" "_" r1 r2
      KnownSnPGP g         -> shorthand g

instance NNomenclature a => NNomenclature (PGP a) where
  nNomenclature =
    \case
      ClassLevelPGP n      -> "PGP (" <> show n <> ")"
      CombinedRadylsPGP rs -> "PGP " <> nNomenclature rs
      UnknownSnPGP r1 r2   -> renderDiradylPL nNomenclature "PGP" "_" r1 r2
      KnownSnPGP g         -> nNomenclature g

instance Shorthand a => Shorthand (PI a) where
  shorthand =
    \case
      ClassLevelPI n      -> "PI (" <> show n <> ")"
      CombinedRadylsPI rs -> "PI " <> shorthand rs
      UnknownSnPI r1 r2   -> renderDiradylPL shorthand "PI" "_" r1 r2
      KnownSnPI g         -> shorthand g

instance NNomenclature a => NNomenclature (PI a) where
  nNomenclature =
    \case
      ClassLevelPI n      -> "PI (" <> show n <> ")"
      CombinedRadylsPI rs -> "PI " <> nNomenclature rs
      UnknownSnPI r1 r2   -> renderDiradylPL nNomenclature "PI" "_" r1 r2
      KnownSnPI g         -> nNomenclature g

instance Shorthand a => Shorthand (PIP a) where
  shorthand =
    \case
      ClassLevelPIP n        ->   "PIP (" <> show n <> ")"
      CombinedRadylsPIP rs p ->   "PIP" <> renderPO4 p <> " " <> shorthand rs
      UnknownSnPIP r1 r2 p   -> renderPIPs shorthand renderPO4 "_" r1 r2 p
      KnownSnPIP g           -> shorthand g

instance NNomenclature a => NNomenclature (PIP a) where
  nNomenclature =
    \case
      ClassLevelPIP n        ->   "PIP (" <> show n <> ")"
      CombinedRadylsPIP rs p ->   "PIP" <> renderPO4 p <> " " <> nNomenclature rs
      UnknownSnPIP r1 r2 p   -> renderPIPs nNomenclature renderPO4 "_" r1 r2 p
      KnownSnPIP g           -> nNomenclature g

instance Shorthand a => Shorthand (PIP2 a) where
  shorthand =
    \case
      ClassLevelPIP2 n         -> "PIP2 (" <> show n <> ")"
      CombinedRadylsPIP2 rs ps -> "PIP2" <> renderPO4s ps <> " " <> shorthand rs
      UnknownSnPIP2 r1 r2 ps   -> renderPIPs shorthand renderPO4s "_" r1 r2 ps
      KnownSnPIP2 g            -> shorthand g

instance NNomenclature a => NNomenclature (PIP2 a) where
  nNomenclature =
    \case
      ClassLevelPIP2 n         -> "PIP (" <> show n <> ")"
      CombinedRadylsPIP2 rs ps -> "PIP" <> renderPO4s ps <> " " <> nNomenclature rs
      UnknownSnPIP2 r1 r2 ps   -> renderPIPs nNomenclature renderPO4s "_" r1 r2 ps
      KnownSnPIP2 g            -> nNomenclature g

renderPIPs f1 f2 sep r1 r2 p  = "PIP" <> f2 p <> " " <> r1' <> "_" <> r2'
    where r1' = f1 r1
          r2' = f1 r2

renderPO4 p =
  case p of
    Nothing  -> ""
    (Just p') -> wrapBrackets $ shorthand p'

renderPO4s ps =
  case ps of
    (Just p1, Just p2) -> wrapBrackets $ shorthand p1 <> "," <> shorthand p2
    (_, _)  -> ""

instance Shorthand a => Shorthand (PIP3 a) where
  shorthand =
    \case
      ClassLevelPIP3 n      -> "PIP3 (" <> show n <> ")"
      CombinedRadylsPIP3 rs -> "PIP3 " <> shorthand rs
      UnknownSnPIP3 r1 r2   -> renderDiradylPL shorthand "PS" "_" r1 r2
      KnownSnPIP3 g         -> shorthand g

instance NNomenclature a => NNomenclature (PIP3 a) where
  nNomenclature =
    \case
      ClassLevelPIP3 n      -> "PIP3 (" <> show n <> ")"
      CombinedRadylsPIP3 rs -> "PIP3 " <> nNomenclature rs
      UnknownSnPIP3 r1 r2   -> renderDiradylPL nNomenclature "PS" "_" r1 r2
      KnownSnPIP3 g         -> nNomenclature g

instance Shorthand a => Shorthand (PS a) where
  shorthand =
    \case
      ClassLevelPS n      -> "PS (" <> show n <> ")"
      CombinedRadylsPS rs -> "PS " <> shorthand rs
      UnknownSnPS r1 r2   -> renderDiradylPL shorthand "PS" "_" r1 r2
      KnownSnPS g         -> shorthand g

instance NNomenclature a => NNomenclature (PS a) where
  nNomenclature =
    \case
      ClassLevelPS n      -> "PS (" <> show n <> ")"
      CombinedRadylsPS rs -> "PS " <> nNomenclature rs
      UnknownSnPS r1 r2   -> renderDiradylPL nNomenclature "PS" "_" r1 r2
      KnownSnPS g         -> nNomenclature g

-- instance Shorthand CL where
--     shorthand l =
--       case l of
--         (ClassLevelCL n)          -> "CL (" <> show n <> ")"
--         (CombinedRadylsCL rs)     -> "CL " <> shorthand rs
--         (UnknownSnCL r1 r2 r3 r4) -> renderCL shorthand "_" r1 r2 r3 r4
--         (KnownSnCL r1 r2 r3 r4)   -> renderCL shorthand "/" r1 r2 r3 r4
--
-- instance NNomenclature CL where
--     nNomenclature l =
--       case l of
--         (ClassLevelCL n)          -> "CL (" <> show n <> ")"
--         (CombinedRadylsCL rs)     -> "CL " <> nNomenclature rs
--         (UnknownSnCL r1 r2 r3 r4) -> renderCL nNomenclature "_" r1 r2 r3 r4
--         (KnownSnCL r1 r2 r3 r4)   -> renderCL nNomenclature "/" r1 r2 r3 r4

renderCL f sep r1 r2 r3 r4 = "CL " <> r1' <> sep <> r2' <> sep <> r3'<> sep <> r4'
    where r1' = f r1
          r2' = f r2
          r3' = f r3
          r4' = f r4

-- instance Shorthand BMP where
--     shorthand l =
--       case l of
--         (ClassLevelBMP n)      -> "BMP (" <> show n <> ")"
--         (CombinedRadylsBMP rs) -> "BMP " <> shorthand rs
--         (BMP r1 r2)            -> renderDiradylPL shorthand "BMP" "/" r1 r2
--
-- instance NNomenclature BMP where
--     nNomenclature l =
--       case l of
--         (ClassLevelBMP n)      -> "BMP (" <> show n <> ")"
--         (CombinedRadylsBMP rs) -> "BMP " <> nNomenclature rs
--         (BMP r1 r2)            -> renderDiradylPL nNomenclature "BMP" "/" r1 r2

renderDiradylPL f h sep r1 r2 = h <> " " <> r1' <> sep <> r2'
    where r1' = f r1
          r2' = f r2
