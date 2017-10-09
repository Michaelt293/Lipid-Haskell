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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Lipid.Glycerophospholipid where

import Lipid.Blocks
import Lipid.Format
import qualified Lipid.ClassLevel.Glycerophospholipid as ClassLevel.Glycerophospholipid
import qualified Lipid.CombinedRadyl.Glycerophospholipid as CombinedRadyl.Glycerophospholipid
import qualified Lipid.UnknownSn.Glycerophospholipid as UnknownSn.Glycerophospholipid
import qualified Lipid.KnownSn.Glycerophospholipid as KnownSn.Glycerophospholipid
import Control.Lens
import Data.Monoid ((<>))


data PA a
  = ClassLevelPA       ClassLevel.Glycerophospholipid.PA
  | CombinedRadylsPA   (CombinedRadyl.Glycerophospholipid.PA a)
  | UnknownSnPA        (UnknownSn.Glycerophospholipid.PA a)
  | KnownSnPA          (KnownSn.Glycerophospholipid.PA a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makePrisms ''PA

data PC a
  = ClassLevelPC       ClassLevel.Glycerophospholipid.PC
  | CombinedRadylsPC   (CombinedRadyl.Glycerophospholipid.PC a)
  | UnknownSnPC        (UnknownSn.Glycerophospholipid.PC a)
  | KnownSnPC          (KnownSn.Glycerophospholipid.PC a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makePrisms ''PC

data PE a
  = ClassLevelPE       ClassLevel.Glycerophospholipid.PE
  | CombinedRadylsPE   (CombinedRadyl.Glycerophospholipid.PE a)
  | UnknownSnPE        (UnknownSn.Glycerophospholipid.PE a)
  | KnownSnPE          (KnownSn.Glycerophospholipid.PE a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makePrisms ''ClassLevel

data PG a
  = ClassLevelPG       ClassLevel.Glycerophospholipid.PG
  | CombinedRadylsPG   (CombinedRadyl.Glycerophospholipid.PG a)
  | UnknownSnPG        (UnknownSn.Glycerophospholipid.PG a)
  | KnownSnPG          (KnownSn.Glycerophospholipid.PG a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makePrisms ''PG

data PGP a
  = ClassLevelPGP      ClassLevel.Glycerophospholipid.PGP
  | CombinedRadylsPGP  (CombinedRadyl.Glycerophospholipid.PGP a)
  | UnknownSnPGP       (UnknownSn.Glycerophospholipid.PGP a)
  | KnownSnPGP         (KnownSn.Glycerophospholipid.PGP a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makePrisms ''PGP

data PI a
  = ClassLevelPI       ClassLevel.Glycerophospholipid.PI
  | CombinedRadylsPI   (CombinedRadyl.Glycerophospholipid.PI a)
  | UnknownSnPI        (UnknownSn.Glycerophospholipid.PI a)
  | KnownSnPI          (KnownSn.Glycerophospholipid.PI a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makePrisms ''PI

data PIP a
  = ClassLevelPIP      ClassLevel.Glycerophospholipid.PIP
  | CombinedRadylsPIP  (CombinedRadyl.Glycerophospholipid.PIP a)
  | UnknownSnPIP       (UnknownSn.Glycerophospholipid.PIP a)
  | KnownSnPIP         (KnownSn.Glycerophospholipid.PIP a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makePrisms ''PIP

data PIP2 a
  = ClassLevelPIP2     ClassLevel.Glycerophospholipid.PIP2
  | CombinedRadylsPIP2 (CombinedRadyl.Glycerophospholipid.PIP2 a)
  | UnknownSnPIP2      (UnknownSn.Glycerophospholipid.PIP2 a)
  | KnownSnPIP2        (KnownSn.Glycerophospholipid.PIP2 a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makePrisms ''PIP2

data PIP3 a
  = ClassLevelPIP3     ClassLevel.Glycerophospholipid.PIP3
  | CombinedRadylsPIP3 (CombinedRadyl.Glycerophospholipid.PIP3 a)
  | UnknownSnPIP3      (UnknownSn.Glycerophospholipid.PIP3 a)
  | KnownSnPIP3        (KnownSn.Glycerophospholipid.PIP3 a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makePrisms ''PIP3

data PS a
  = ClassLevelPS       ClassLevel.Glycerophospholipid.PS
  | CombinedRadylsPS   (CombinedRadyl.Glycerophospholipid.PS a)
  | UnknownSnPS        (UnknownSn.Glycerophospholipid.PS a)
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


instance (Shorthand (CarbonChain a), Shorthand (TwoCombinedChains a)) => Shorthand (PA a) where
  shorthand =
    \case
      ClassLevelPA n      -> shorthand n
      CombinedRadylsPA rs -> shorthand rs
      UnknownSnPA rs      -> shorthand rs
      KnownSnPA g         -> shorthand g

instance (NNomenclature (CarbonChain a), NNomenclature (TwoCombinedChains a)) => NNomenclature (PA a) where
  nNomenclature =
    \case
      ClassLevelPA n      -> shorthand n
      CombinedRadylsPA rs -> nNomenclature rs
      UnknownSnPA rs      -> nNomenclature rs
      KnownSnPA g         -> nNomenclature g

instance (Shorthand (CarbonChain a), Shorthand (TwoCombinedChains a)) => Shorthand (PC a) where
  shorthand =
    \case
      ClassLevelPC n      -> shorthand n
      CombinedRadylsPC rs -> shorthand rs
      UnknownSnPC rs      -> shorthand rs
      KnownSnPC g         -> shorthand g

instance (NNomenclature (CarbonChain a), NNomenclature (TwoCombinedChains a)) => NNomenclature (PC a) where
  nNomenclature =
    \case
      ClassLevelPC n      -> shorthand n
      CombinedRadylsPC rs -> nNomenclature rs
      UnknownSnPC rs      -> nNomenclature rs
      KnownSnPC g         -> nNomenclature g

instance (Shorthand (CarbonChain a), Shorthand (TwoCombinedChains a)) => Shorthand (PE a) where
  shorthand =
    \case
      ClassLevelPE n      -> shorthand n
      CombinedRadylsPE rs -> shorthand rs
      UnknownSnPE rs      -> shorthand rs
      KnownSnPE g         -> shorthand g

instance (NNomenclature (CarbonChain a), NNomenclature (TwoCombinedChains a)) => NNomenclature (PE a) where
  nNomenclature =
    \case
      ClassLevelPE n      -> shorthand n
      CombinedRadylsPE rs -> nNomenclature rs
      UnknownSnPE rs      -> nNomenclature rs
      KnownSnPE g         -> nNomenclature g

instance (Shorthand (CarbonChain a), Shorthand (TwoCombinedChains a)) => Shorthand (PG a) where
  shorthand =
    \case
      ClassLevelPG n      -> shorthand n
      CombinedRadylsPG rs -> shorthand rs
      UnknownSnPG rs      -> shorthand rs
      KnownSnPG g         -> shorthand g

instance (NNomenclature (CarbonChain a), NNomenclature (TwoCombinedChains a)) => NNomenclature (PG a) where
  nNomenclature =
    \case
      ClassLevelPG n      -> shorthand n
      CombinedRadylsPG rs -> nNomenclature rs
      UnknownSnPG rs      -> nNomenclature rs
      KnownSnPG g         -> nNomenclature g

instance (Shorthand (CarbonChain a), Shorthand (TwoCombinedChains a)) => Shorthand (PGP a) where
  shorthand =
    \case
      ClassLevelPGP n      -> shorthand n
      CombinedRadylsPGP rs -> shorthand rs
      UnknownSnPGP rs      -> shorthand rs
      KnownSnPGP g         -> shorthand g

instance (NNomenclature (CarbonChain a), NNomenclature (TwoCombinedChains a)) => NNomenclature (PGP a) where
  nNomenclature =
    \case
      ClassLevelPGP n      -> shorthand n
      CombinedRadylsPGP rs -> nNomenclature rs
      UnknownSnPGP rs      -> nNomenclature rs
      KnownSnPGP g         -> nNomenclature g

instance (Shorthand (CarbonChain a), Shorthand (TwoCombinedChains a)) => Shorthand (PI a) where
  shorthand =
    \case
      ClassLevelPI n      -> shorthand n
      CombinedRadylsPI rs -> shorthand rs
      UnknownSnPI rs      -> shorthand rs
      KnownSnPI g         -> shorthand g

instance (NNomenclature (CarbonChain a), NNomenclature (TwoCombinedChains a)) => NNomenclature (PI a) where
  nNomenclature =
    \case
      ClassLevelPI n      -> shorthand n
      CombinedRadylsPI rs -> nNomenclature rs
      UnknownSnPI rs      -> nNomenclature rs
      KnownSnPI g         -> nNomenclature g

instance (Shorthand (CarbonChain a), Shorthand (TwoCombinedChains a)) => Shorthand (PIP a) where
  shorthand =
    \case
      ClassLevelPIP n      -> shorthand n
      CombinedRadylsPIP rs -> shorthand rs
      UnknownSnPIP rs      -> shorthand rs
      KnownSnPIP g         -> shorthand g

instance (NNomenclature (CarbonChain a), NNomenclature (TwoCombinedChains a)) => NNomenclature (PIP a) where
  nNomenclature =
    \case
      ClassLevelPIP n      -> shorthand n
      CombinedRadylsPIP rs -> nNomenclature rs
      UnknownSnPIP rs      -> nNomenclature rs
      KnownSnPIP g         -> nNomenclature g

instance (Shorthand (CarbonChain a), Shorthand (TwoCombinedChains a)) => Shorthand (PIP2 a) where
  shorthand =
    \case
      ClassLevelPIP2 n      -> shorthand n
      CombinedRadylsPIP2 rs -> shorthand rs
      UnknownSnPIP2 rs      -> shorthand rs
      KnownSnPIP2 g         -> shorthand g

instance (NNomenclature (CarbonChain a), NNomenclature (TwoCombinedChains a)) => NNomenclature (PIP2 a) where
  nNomenclature =
    \case
      ClassLevelPIP2 n      -> shorthand n
      CombinedRadylsPIP2 rs -> nNomenclature rs
      UnknownSnPIP2 rs      -> nNomenclature rs
      KnownSnPIP2 g         -> nNomenclature g

instance (Shorthand (CarbonChain a), Shorthand (TwoCombinedChains a)) => Shorthand (PIP3 a) where
  shorthand =
    \case
      ClassLevelPIP3 n      -> shorthand n
      CombinedRadylsPIP3 rs -> shorthand rs
      UnknownSnPIP3 rs      -> shorthand rs
      KnownSnPIP3 g         -> shorthand g

instance (NNomenclature (CarbonChain a), NNomenclature (TwoCombinedChains a)) => NNomenclature (PIP3 a) where
  nNomenclature =
    \case
      ClassLevelPIP3 n      -> shorthand n
      CombinedRadylsPIP3 rs -> nNomenclature rs
      UnknownSnPIP3 rs      -> nNomenclature rs
      KnownSnPIP3 g         -> nNomenclature g

instance (Shorthand (CarbonChain a), Shorthand (TwoCombinedChains a)) => Shorthand (PS a) where
  shorthand =
    \case
      ClassLevelPS n      -> shorthand n
      CombinedRadylsPS rs -> shorthand rs
      UnknownSnPS rs      -> shorthand rs
      KnownSnPS g         -> shorthand g

instance (NNomenclature (CarbonChain a), NNomenclature (TwoCombinedChains a)) => NNomenclature (PS a) where
  nNomenclature =
    \case
      ClassLevelPS n      -> shorthand n
      CombinedRadylsPS rs -> nNomenclature rs
      UnknownSnPS rs      -> nNomenclature rs
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
