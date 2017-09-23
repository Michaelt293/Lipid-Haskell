{-|
Module      : Lipid.UnknownSn.Glycerolipid.hs
Description :
Copyright   : Michael Thomas
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Lipid.UnknownSn.Glycerolipid where

import Lipid.Blocks
import Lipid.Format
import Control.Lens
import Data.Monoid ((<>))
import Data.List (sort)

newtype MG a = MG
  { _radylMG :: Radyl a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeClassy ''MG

newtype DG a = DG
  { _twoRadylsDG :: TwoRadyls a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeClassy ''DG

instance HasTwoRadyls (DG a) a where
  twoRadyls = twoRadylsDG

instance AllRadyls DG where
  allRadyls f (DG (TwoRadyls r1 r2)) =
    (\x y -> DG (TwoRadyls x y)) <$> f r1 <*> f r2

newtype TG a = TG
  { _threeRadylsTG :: ThreeRadyls a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeClassy ''TG

instance HasThreeRadyls (TG a) a where
  threeRadyls = threeRadylsTG

instance AllRadyls TG where
  allRadyls f (TG (ThreeRadyls r1 r2 r3)) =
    (\x y z -> TG (ThreeRadyls x y z)) <$> f r1 <*> f r2 <*> f r3

instance Shorthand a => Shorthand (MG a) where
  shorthand (MG r) = "MG " <> shorthand r

instance NNomenclature a => NNomenclature (MG a) where
    nNomenclature (MG r) = "MG " <> nNomenclature r

instance Shorthand a => Shorthand (DG a) where
    shorthand (DG rs) = "DG " <> shorthand rs

instance NNomenclature a => NNomenclature (DG a) where
    nNomenclature (DG rs) = "DG " <> nNomenclature rs

instance Shorthand a => Shorthand (TG a) where
  shorthand (TG rs) = "TG " <> shorthand rs

instance NNomenclature a => NNomenclature (TG a) where
  nNomenclature (TG rs) = "TG " <> nNomenclature rs
