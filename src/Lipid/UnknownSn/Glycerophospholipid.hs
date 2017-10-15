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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Lipid.UnknownSn.Glycerophospholipid where

import Lipid.Blocks
import Control.Lens
import Data.Monoid ((<>))

newtype PA a = PA
  { _getTwoRadylsPA :: TwoRadyls a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeClassy ''PA

instance Shorthand (TwoRadyls a) => Shorthand (PA a) where
  shorthand (PA rs) = "PA " <> shorthand rs

instance NNomenclature (TwoRadyls a) => NNomenclature (PA a) where
  nNomenclature (PA rs) = "PA " <> nNomenclature rs

instance AllRadyls PA where
  allRadyls f (PA (TwoRadyls r1 r2)) =
    (\x y -> PA (TwoRadyls x y)) <$> f r1 <*> f r2

instance HasTwoRadyls (PA a) a where
  twoRadyls = getTwoRadylsPA

newtype PE a = PE
  { _getTwoRadylsPE :: TwoRadyls a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeClassy ''PE

instance Shorthand (TwoRadyls a) => Shorthand (PE a) where
  shorthand (PE rs) = "PE " <> shorthand rs

instance NNomenclature (TwoRadyls a) => NNomenclature (PE a) where
  nNomenclature (PE rs) = "PE " <> nNomenclature rs

instance AllRadyls PE where
  allRadyls f (PE (TwoRadyls r1 r2)) =
    (\x y -> PE (TwoRadyls x y)) <$> f r1 <*> f r2

instance HasTwoRadyls (PE a) a where
  twoRadyls = getTwoRadylsPE

newtype PC a = PC
  { _getTwoRadylsPC :: TwoRadyls a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeClassy ''PC

instance Shorthand (TwoRadyls a) => Shorthand (PC a) where
  shorthand (PC rs) = "PC " <> shorthand rs

instance NNomenclature (TwoRadyls a) => NNomenclature (PC a) where
  nNomenclature (PC rs) = "PC " <> nNomenclature rs

instance AllRadyls PC where
  allRadyls f (PC (TwoRadyls r1 r2)) =
    (\x y -> PC (TwoRadyls x y)) <$> f r1 <*> f r2

instance HasTwoRadyls (PC a) a where
  twoRadyls = getTwoRadylsPC

newtype PG a = PG
  { _getTwoRadylsPG :: TwoRadyls a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeClassy ''PG

instance Shorthand (TwoRadyls a) => Shorthand (PG a) where
  shorthand (PG rs) = "PG " <> shorthand rs

instance NNomenclature (TwoRadyls a) => NNomenclature (PG a) where
  nNomenclature (PG rs) = "PG " <> nNomenclature rs

instance AllRadyls PG where
  allRadyls f (PG (TwoRadyls r1 r2)) =
    (\x y -> PG (TwoRadyls x y)) <$> f r1 <*> f r2

instance HasTwoRadyls (PG a) a where
  twoRadyls = getTwoRadylsPG

newtype PGP a = PGP
  { _getTwoRadylsPGP :: TwoRadyls a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeClassy ''PGP

instance Shorthand (TwoRadyls a) => Shorthand (PGP a) where
  shorthand (PGP rs) = "PGP " <> shorthand rs

instance NNomenclature (TwoRadyls a) => NNomenclature (PGP a) where
  nNomenclature (PGP rs) = "PGP " <> nNomenclature rs

instance AllRadyls PGP where
  allRadyls f (PGP (TwoRadyls r1 r2)) =
    (\x y -> PGP (TwoRadyls x y)) <$> f r1 <*> f r2

instance HasTwoRadyls (PGP a) a where
  twoRadyls = getTwoRadylsPGP

newtype PI a = PI
  { _getTwoRadylsPI :: TwoRadyls a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeClassy ''PI

instance Shorthand (TwoRadyls a) => Shorthand (PI a) where
  shorthand (PI rs) = "PI " <> shorthand rs

instance NNomenclature (TwoRadyls a) => NNomenclature (PI a) where
  nNomenclature (PI rs) = "PI " <> nNomenclature rs

instance AllRadyls PI where
  allRadyls f (PI (TwoRadyls r1 r2)) =
    (\x y -> PI (TwoRadyls x y)) <$> f r1 <*> f r2

instance HasTwoRadyls (PI a) a where
  twoRadyls = getTwoRadylsPI

newtype PS a = PS
  { _getTwoRadylsPS :: TwoRadyls a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeClassy ''PS

instance Shorthand (TwoRadyls a) => Shorthand (PS a) where
  shorthand (PS rs) = "PS " <> shorthand rs

instance NNomenclature (TwoRadyls a) => NNomenclature (PS a) where
  nNomenclature (PS rs) = "PS " <> nNomenclature rs

instance AllRadyls PS where
  allRadyls f (PS (TwoRadyls r1 r2)) =
    (\x y -> PS (TwoRadyls x y)) <$> f r1 <*> f r2

instance HasTwoRadyls (PS a) a where
  twoRadyls = getTwoRadylsPS

newtype PIP3 a = PIP3
  { _getTwoRadylsPIP3 :: TwoRadyls a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeClassy ''PIP3

instance Shorthand (TwoRadyls a) => Shorthand (PIP3 a) where
  shorthand (PIP3 rs) = "PIP3 " <> shorthand rs

instance NNomenclature (TwoRadyls a) => NNomenclature (PIP3 a) where
  nNomenclature (PIP3 rs) = "PIP3 " <> nNomenclature rs

instance AllRadyls PIP3 where
  allRadyls f (PIP3 (TwoRadyls r1 r2)) =
    (\x y -> PIP3 (TwoRadyls x y)) <$> f r1 <*> f r2

instance HasTwoRadyls (PIP3 a) a where
  twoRadyls = getTwoRadylsPIP3

data PIP a = PIP
  { _getHeadgroupPIP :: PhosphatidylinositolMonophosphate
  , _getTwoRadylsPIP :: TwoRadyls a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeClassy ''PIP

instance Shorthand (TwoRadyls a) => Shorthand (PIP a) where
  shorthand (PIP h rs) = shorthand h <> " " <> shorthand rs

instance NNomenclature (TwoRadyls a) => NNomenclature (PIP a) where
  nNomenclature (PIP h rs) = shorthand h <> " " <> nNomenclature rs

instance AllRadyls PIP where
  allRadyls f (PIP h (TwoRadyls r1 r2)) =
    (\x y -> PIP h (TwoRadyls x y)) <$> f r1 <*> f r2

instance HasTwoRadyls (PIP a) a where
  twoRadyls = getTwoRadylsPIP

data PIP2 a = PIP2
  { _getHeadgroupPIP2 :: PhosphatidylinositolBisphosphate
  , _getTwoRadylsPIP2 :: TwoRadyls a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeClassy ''PIP2

instance Shorthand (TwoRadyls a) => Shorthand (PIP2 a) where
  shorthand (PIP2 h rs) = shorthand h <> " " <> shorthand rs

instance NNomenclature (TwoRadyls a) => NNomenclature (PIP2 a) where
  nNomenclature (PIP2 h rs) = shorthand h <> " " <> nNomenclature rs

instance AllRadyls PIP2 where
  allRadyls f (PIP2 h (TwoRadyls r1 r2)) =
    (\x y -> PIP2 h (TwoRadyls x y)) <$> f r1 <*> f r2

instance HasTwoRadyls (PIP2 a) a where
  twoRadyls = getTwoRadylsPIP2
