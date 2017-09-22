{-|
Module      : Lipid.UnknownSn.Glycerophospholipid
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


module Lipid.UnknownSn.Glycerophospholipid where

import Lipid.Blocks
import Control.Lens
import Data.Monoid ((<>))

newtype PA a = PA
  { _getTwoRadylsPA :: TwoRadyls a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeClassy ''PA

instance Shorthand a => Shorthand (PA a) where
  shorthand (PA rs) = "PA " <> shorthand rs

instance NNomenclature a => NNomenclature (PA a) where
  nNomenclature (PA rs) = "PA " <> nNomenclature rs

instance HasTwoRadyls (PA a) a where
  twoRadyls = getTwoRadylsPA

newtype PE a = PE
  { _getTwoRadylsPE :: TwoRadyls a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeClassy ''PE

instance Shorthand a => Shorthand (PE a) where
  shorthand (PE rs) = "PE " <> shorthand rs

instance NNomenclature a => NNomenclature (PE a) where
  nNomenclature (PE rs) = "PE " <> nNomenclature rs

instance HasTwoRadyls (PE a) a where
  twoRadyls = getTwoRadylsPE

newtype PC a = PC
  { _getTwoRadylsPC :: TwoRadyls a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeClassy ''PC

instance Shorthand a => Shorthand (PC a) where
  shorthand (PC rs) = "PC " <> shorthand rs

instance NNomenclature a => NNomenclature (PC a) where
  nNomenclature (PC rs) = "PC " <> nNomenclature rs

instance HasTwoRadyls (PC a) a where
  twoRadyls = getTwoRadylsPC

newtype PG a = PG
  { _getTwoRadylsPG :: TwoRadyls a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeClassy ''PG

instance Shorthand a => Shorthand (PG a) where
  shorthand (PG rs) = "PG " <> shorthand rs

instance NNomenclature a => NNomenclature (PG a) where
  nNomenclature (PG rs) = "PG " <> nNomenclature rs

instance HasTwoRadyls (PG a) a where
  twoRadyls = getTwoRadylsPG

newtype PGP a = PGP
  { _getTwoRadylsPGP :: TwoRadyls a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeClassy ''PGP

instance Shorthand a => Shorthand (PGP a) where
  shorthand (PGP rs) = "PGP " <> shorthand rs

instance NNomenclature a => NNomenclature (PGP a) where
  nNomenclature (PGP rs) = "PGP " <> nNomenclature rs

instance HasTwoRadyls (PGP a) a where
  twoRadyls = getTwoRadylsPGP

newtype PI a = PI
  { _getTwoRadylsPI :: TwoRadyls a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeClassy ''PI

instance Shorthand a => Shorthand (PI a) where
  shorthand (PI rs) = "PI " <> shorthand rs

instance NNomenclature a => NNomenclature (PI a) where
  nNomenclature (PI rs) = "PI " <> nNomenclature rs

instance HasTwoRadyls (PI a) a where
  twoRadyls = getTwoRadylsPI

newtype PS a = PS
  { _getTwoRadylsPS :: TwoRadyls a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeClassy ''PS

instance Shorthand a => Shorthand (PS a) where
  shorthand (PS rs) = "PS " <> shorthand rs

instance NNomenclature a => NNomenclature (PS a) where
  nNomenclature (PS rs) = "PS " <> nNomenclature rs

instance HasTwoRadyls (PS a) a where
  twoRadyls = getTwoRadylsPS

newtype PIP3 a = PIP3
  { _getTwoRadylsPIP3 :: TwoRadyls a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeClassy ''PIP3

instance Shorthand a => Shorthand (PIP3 a) where
  shorthand (PIP3 rs) = "PIP3 " <> shorthand rs

instance NNomenclature a => NNomenclature (PIP3 a) where
  nNomenclature (PIP3 rs) = "PIP3 " <> nNomenclature rs

instance HasTwoRadyls (PIP3 a) a where
  twoRadyls = getTwoRadylsPIP3

data PIP a = PIP
  { _getHeadgroupPIP :: PhosphatidylinositolMonophosphate
  , _getTwoRadylsPIP :: TwoRadyls a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeClassy ''PIP

instance Shorthand a => Shorthand (PIP a) where
  shorthand (PIP h rs) = shorthand h <> shorthand rs

instance NNomenclature a => NNomenclature (PIP a) where
  nNomenclature (PIP h rs) = shorthand h <> nNomenclature rs

instance HasTwoRadyls (PIP a) a where
  twoRadyls = getTwoRadylsPIP

data PIP2 a = PIP2
  { _getHeadgroupPIP2 :: PhosphatidylinositolBisphosphate
  , _getTwoRadylsPIP2 :: TwoRadyls a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeClassy ''PIP2

instance Shorthand a => Shorthand (PIP2 a) where
  shorthand (PIP2 h rs) = shorthand h <> shorthand rs

instance NNomenclature a => NNomenclature (PIP2 a) where
  nNomenclature (PIP2 h rs) = shorthand h <> nNomenclature rs

instance HasTwoRadyls (PIP2 a) a where
  twoRadyls = getTwoRadylsPIP2
