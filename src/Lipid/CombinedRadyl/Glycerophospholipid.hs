{-|
Module      : Lipid.CombinedRadyl.Glycerophospholipid
Description :
Copyright   : Michael Thomas
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental
-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}


module Lipid.CombinedRadyl.Glycerophospholipid where

import Isotope
import Lipid.Blocks
import Control.Lens
import Data.Monoid ((<>))

newtype PA a = PA
  { _twoCombinedRadylsPA :: TwoCombinedRadyls a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeClassy ''PA

instance Shorthand a => Shorthand (PA a) where
  shorthand (PA rs) = "PA " <> shorthand rs

instance NNomenclature a => NNomenclature (PA a) where
  nNomenclature (PA rs) = "PA " <> nNomenclature rs

instance HasTwoCombinedRadyls (PA a) a where
  twoCombinedRadyls = twoCombinedRadylsPA

instance ToElementalComposition (PA a) where
  toElementalComposition (PA rs) = toElementalComposition rs
  charge _ = Just 0

newtype PE a = PE
  { _twoCombinedRadylsPE :: TwoCombinedRadyls a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeClassy ''PE

instance Shorthand a => Shorthand (PE a) where
  shorthand (PE rs) = "PE " <> shorthand rs

instance NNomenclature a => NNomenclature (PE a) where
  nNomenclature (PE rs) = "PE " <> nNomenclature rs

instance HasTwoCombinedRadyls (PE a) a where
  twoCombinedRadyls = twoCombinedRadylsPE

instance ToElementalComposition (PE a) where
  toElementalComposition (PE rs) = toElementalComposition rs
  charge _ = Just 0

newtype PC a = PC
  { _twoCombinedRadylsPC :: TwoCombinedRadyls a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeClassy ''PC

instance Shorthand a => Shorthand (PC a) where
  shorthand (PC rs) = "PC " <> shorthand rs

instance NNomenclature a => NNomenclature (PC a) where
  nNomenclature (PC rs) = "PC " <> nNomenclature rs

instance HasTwoCombinedRadyls (PC a) a where
  twoCombinedRadyls = twoCombinedRadylsPC

instance ToElementalComposition (PC a) where
  toElementalComposition (PC rs) = toElementalComposition rs
  charge _ = Just 0

newtype PG a = PG
  { _twoCombinedRadylsPG :: TwoCombinedRadyls a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeClassy ''PG

instance Shorthand a => Shorthand (PG a) where
  shorthand (PG rs) = "PG " <> shorthand rs

instance NNomenclature a => NNomenclature (PG a) where
  nNomenclature (PG rs) = "PG " <> nNomenclature rs

instance HasTwoCombinedRadyls (PG a) a where
  twoCombinedRadyls = twoCombinedRadylsPG

instance ToElementalComposition (PG a) where
  toElementalComposition (PG rs) = toElementalComposition rs
  charge _ = Just 0

newtype PS a = PS
  { _twoCombinedRadylsPS :: TwoCombinedRadyls a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeClassy ''PS

instance Shorthand a => Shorthand (PS a) where
  shorthand (PS rs) = "PS " <> shorthand rs

instance NNomenclature a => NNomenclature (PS a) where
  nNomenclature (PS rs) = "PS " <> nNomenclature rs

instance HasTwoCombinedRadyls (PS a) a where
  twoCombinedRadyls = twoCombinedRadylsPS

instance ToElementalComposition (PS a) where
  toElementalComposition (PS rs) = toElementalComposition rs
  charge _ = Just 0

newtype PI a = PI
  { _twoCombinedRadylsPI :: TwoCombinedRadyls a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeClassy ''PI

instance Shorthand a => Shorthand (PI a) where
  shorthand (PI rs) = "PI " <> shorthand rs

instance NNomenclature a => NNomenclature (PI a) where
  nNomenclature (PI rs) = "PI " <> nNomenclature rs

instance HasTwoCombinedRadyls (PI a) a where
  twoCombinedRadyls = twoCombinedRadylsPI

instance ToElementalComposition (PI a) where
  toElementalComposition (PI rs) = toElementalComposition rs
  charge _ = Just 0

newtype PGP a = PGP
  { _twoCombinedRadylsPGP :: TwoCombinedRadyls a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeClassy ''PGP

instance Shorthand a => Shorthand (PGP a) where
  shorthand (PGP rs) = "PGP " <> shorthand rs

instance NNomenclature a => NNomenclature (PGP a) where
  nNomenclature (PGP rs) = "PGP " <> nNomenclature rs

instance HasTwoCombinedRadyls (PGP a) a where
  twoCombinedRadyls = twoCombinedRadylsPGP

instance ToElementalComposition (PGP a) where
  toElementalComposition (PGP rs) = toElementalComposition rs
  charge _ = Just 0

newtype PIP3 a = PIP3
  { _twoCombinedRadylsPIP3 :: TwoCombinedRadyls a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeClassy ''PIP3

instance Shorthand a => Shorthand (PIP3 a) where
  shorthand (PIP3 rs) = "PIP3 " <> shorthand rs

instance NNomenclature a => NNomenclature (PIP3 a) where
  nNomenclature (PIP3 rs) = "PIP3 " <> nNomenclature rs

instance HasTwoCombinedRadyls (PIP3 a) a where
  twoCombinedRadyls = twoCombinedRadylsPIP3

instance ToElementalComposition (PIP3 a) where
  toElementalComposition (PIP3 rs) = toElementalComposition rs
  charge _ = Just 0

data PIP a = PIP
  { _headgroupPIP         :: PhosphatidylinositolMonophosphate
  , _twoCombinedRadylsPIP :: TwoCombinedRadyls a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeClassy ''PIP

instance Shorthand a => Shorthand (PIP a) where
  shorthand (PIP h rs) = shorthand h <> shorthand rs

instance NNomenclature a => NNomenclature (PIP a) where
  nNomenclature (PIP h rs) = shorthand h <> nNomenclature rs

instance HasTwoCombinedRadyls (PIP a) a where
  twoCombinedRadyls = twoCombinedRadylsPIP

instance ToElementalComposition (PIP a) where
  toElementalComposition (PIP h rs) =
    toElementalComposition h <> toElementalComposition rs
  charge _ = Just 0

data PIP2 a = PIP2
  { _headgroupPIP2         :: PhosphatidylinositolBisphosphate
  , _twoCombinedRadylsPIP2 :: TwoCombinedRadyls a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeClassy ''PIP2

instance Shorthand a => Shorthand (PIP2 a) where
  shorthand (PIP2 h rs) = shorthand h <> shorthand rs

instance NNomenclature a => NNomenclature (PIP2 a) where
  nNomenclature (PIP2 h rs) = shorthand h <> nNomenclature rs

instance HasTwoCombinedRadyls (PIP2 a) a where
  twoCombinedRadyls = twoCombinedRadylsPIP2

instance ToElementalComposition (PIP2 a) where
  toElementalComposition (PIP2 h rs) =
    toElementalComposition h <> toElementalComposition rs
  charge _ = Just 0
