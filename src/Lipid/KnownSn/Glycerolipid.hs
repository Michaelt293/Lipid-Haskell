{-|
Module      : Lipid.KnownSn.Glycerolipid
Description : Glycerolipid data type and instances of Shorthand and
              NNomenclature defined.
Copyright   : Michael Thomas
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Lipid.KnownSn.Glycerolipid where

import Isotope
import Lipid.Blocks
import Lipid.Format
import Control.Lens
import Control.Applicative
import Data.Monoid ((<>))

newtype TG a = TG
  { _getTG :: Glycerol (Radyl a) (Radyl a) (Radyl a)
  } deriving (Show, Eq, Ord)

makeLenses ''TG

instance Functor TG where
  fmap f (TG (Glycerol r1 r2 r3)) =
    TG $ Glycerol (f <$> r1) (f <$> r2) (f <$> r3)

instance Foldable TG where
  foldMap f (TG (Glycerol r1 r2 r3)) =
    foldMap f r1 <> foldMap f r2 <> foldMap f r3

instance Traversable TG where
  traverse f (TG (Glycerol r1 r2 r3)) =
    (\x y z -> TG (Glycerol x y z))
    <$> traverse f r1
    <*> traverse f r2
    <*> traverse f r3

instance AllRadyls TG where
  allRadyls f (TG (Glycerol r1 r2 r3)) =
    (\x y z -> TG (Glycerol x y z))
    <$> f r1
    <*> f r2
    <*> f r3

instance HasGlycerol (TG a) (Radyl a) (Radyl a) (Radyl a) where
  glycerol = getTG

instance ToElementalComposition (TG a) where
  toElementalComposition (TG g) = toElementalComposition g
  charge (TG g) = charge g

newtype DG12 a = DG12
  { _getDG12 :: Glycerol (Radyl a) (Radyl a) GlycerolHydroxyl
  } deriving (Show, Eq, Ord)

makeLenses ''DG12

instance Functor DG12 where
    fmap f (DG12 (Glycerol r1 r2 GlycerolHydroxyl)) =
      DG12 $ Glycerol (f <$> r1) (f <$> r2) GlycerolHydroxyl

instance Foldable DG12 where
  foldMap f (DG12 (Glycerol r1 r2 _)) =
    foldMap f r1 <> foldMap f r2

instance Traversable DG12 where
  traverse f (DG12 (Glycerol r1 r2 _)) =
    (\x y -> DG12 (Glycerol x y GlycerolHydroxyl))
    <$> traverse f r1
    <*> traverse f r2

instance AllRadyls DG12 where
  allRadyls f (DG12 (Glycerol r1 r2 _)) =
    (\x y -> DG12 (Glycerol x y GlycerolHydroxyl))
    <$> f r1
    <*> f r2

instance HasGlycerol (DG12 a) (Radyl a) (Radyl a) GlycerolHydroxyl where
  glycerol = getDG12

instance ToElementalComposition (DG12 a) where
  toElementalComposition (DG12 g) = toElementalComposition g
  charge (DG12 g) = charge g

newtype DG13 a = DG13
  { _getDG13 :: Glycerol (Radyl a) GlycerolHydroxyl (Radyl a)
  } deriving (Show, Eq, Ord)

makeLenses ''DG13

instance Functor DG13 where
  fmap f (DG13 (Glycerol r1 GlycerolHydroxyl r2)) =
    DG13 $ Glycerol (f <$> r1) GlycerolHydroxyl (f <$> r2)

instance Foldable DG13 where
  foldMap f (DG13 (Glycerol r1 _ r2)) =
    foldMap f r1 <> foldMap f r2

instance Traversable DG13 where
  traverse f (DG13 (Glycerol r1 _ r2)) =
    (\x y -> DG13 (Glycerol x GlycerolHydroxyl y))
    <$> traverse f r1
    <*> traverse f r2

instance AllRadyls DG13 where
  allRadyls f (DG13 (Glycerol r1 _ r2)) =
    (\x y -> DG13 (Glycerol x GlycerolHydroxyl y))
    <$> f r1
    <*> f r2

instance HasGlycerol (DG13 a) (Radyl a) GlycerolHydroxyl (Radyl a) where
  glycerol = getDG13

instance ToElementalComposition (DG13 a) where
  toElementalComposition (DG13 g) = toElementalComposition g
  charge (DG13 g) = charge g

newtype DG23 a = DG23
  { _getDG23 :: Glycerol GlycerolHydroxyl (Radyl a) (Radyl a)
  } deriving (Show, Eq, Ord)

makeLenses ''DG23

instance Functor DG23 where
  fmap f (DG23 (Glycerol GlycerolHydroxyl r1 r2)) =
    DG23 $ Glycerol GlycerolHydroxyl (f <$> r1) (f <$> r2)

instance Foldable DG23 where
  foldMap f (DG23 (Glycerol _ r1 r2)) =
    foldMap f r1 <> foldMap f r2

instance Traversable DG23 where
  traverse f (DG23 (Glycerol _ r1 r2)) =
    (\x y -> DG23 (Glycerol GlycerolHydroxyl x y))
    <$> traverse f r1
    <*> traverse f r2

instance AllRadyls DG23 where
  allRadyls f (DG23 (Glycerol _ r1 r2)) =
    (\x y -> DG23 (Glycerol GlycerolHydroxyl x y))
    <$> f r1
    <*> f r2

instance HasGlycerol (DG23 a) GlycerolHydroxyl (Radyl a) (Radyl a) where
  glycerol = getDG23

instance ToElementalComposition (DG23 a) where
  toElementalComposition (DG23 g) = toElementalComposition g
  charge (DG23 g) = charge g

newtype MG1 a = MG1
  { _getMG1 :: Glycerol (Radyl a) GlycerolHydroxyl GlycerolHydroxyl
  } deriving (Show, Eq, Ord)

makeLenses ''MG1

instance Functor MG1 where
  fmap f (MG1 (Glycerol r GlycerolHydroxyl GlycerolHydroxyl)) =
    MG1 $ Glycerol (f <$> r) GlycerolHydroxyl GlycerolHydroxyl

instance Foldable MG1 where
  foldMap f (MG1 (Glycerol r _ _)) =
    foldMap f r

instance Traversable MG1 where
  traverse f (MG1 (Glycerol r _ _)) =
    (\x -> MG1 (Glycerol x GlycerolHydroxyl GlycerolHydroxyl)) <$> traverse f r

instance AllRadyls MG1 where
  allRadyls f (MG1 (Glycerol r _ _)) =
    (\x -> MG1 (Glycerol x GlycerolHydroxyl GlycerolHydroxyl)) <$> f r

instance HasGlycerol (MG1 a) (Radyl a) GlycerolHydroxyl GlycerolHydroxyl where
  glycerol = getMG1

instance ToElementalComposition (MG1 a) where
  toElementalComposition (MG1 g) = toElementalComposition g
  charge (MG1 g) = charge g

newtype MG2 a = MG2
  { _getMG2 :: Glycerol GlycerolHydroxyl (Radyl a) GlycerolHydroxyl
  } deriving (Show, Eq, Ord)

makeLenses ''MG2

instance Functor MG2 where
  fmap f (MG2 (Glycerol GlycerolHydroxyl r GlycerolHydroxyl)) =
    MG2 $ Glycerol GlycerolHydroxyl (f <$> r) GlycerolHydroxyl

instance Foldable MG2 where
  foldMap f (MG2 (Glycerol _ r _)) = foldMap f r

instance Traversable MG2 where
  traverse f (MG2 (Glycerol _ r _)) =
    (\x -> MG2 (Glycerol GlycerolHydroxyl x GlycerolHydroxyl)) <$> traverse f r

instance AllRadyls MG2 where
  allRadyls f (MG2 (Glycerol _ r _)) =
    (\x -> MG2 (Glycerol GlycerolHydroxyl x GlycerolHydroxyl)) <$> f r

instance HasGlycerol (MG2 a) GlycerolHydroxyl (Radyl a) GlycerolHydroxyl where
  glycerol = getMG2

instance ToElementalComposition (MG2 a) where
  toElementalComposition (MG2 g) = toElementalComposition g
  charge (MG2 g) = charge g

newtype MG3 a = MG3
  { _getMG3 :: Glycerol GlycerolHydroxyl GlycerolHydroxyl (Radyl a)
  } deriving (Show, Eq, Ord)

makeLenses ''MG3

instance Functor MG3 where
  fmap f (MG3 (Glycerol GlycerolHydroxyl GlycerolHydroxyl r)) =
    MG3 $ Glycerol GlycerolHydroxyl GlycerolHydroxyl (f <$> r)

instance Foldable MG3 where
  foldMap f (MG3 (Glycerol _ _ r)) = foldMap f r

instance Traversable MG3 where
  traverse f (MG3 (Glycerol _ _ r)) =
    (MG3 . Glycerol GlycerolHydroxyl GlycerolHydroxyl) <$> traverse f r

instance AllRadyls MG3 where
  allRadyls f (MG3 (Glycerol _ _ r)) =
    (MG3 . Glycerol GlycerolHydroxyl GlycerolHydroxyl) <$> f r

instance HasGlycerol (MG3 a) GlycerolHydroxyl GlycerolHydroxyl (Radyl a) where
  glycerol = getMG3

instance ToElementalComposition (MG3 a) where
  toElementalComposition (MG3 g) = toElementalComposition g
  charge (MG3 g) = charge g

instance Shorthand a => Shorthand (TG a) where
  shorthand (TG (Glycerol r1 r2 r3)) =
    "TG " <> shorthand r1 <> "/" <> shorthand r2 <> "/" <> shorthand r3

instance Shorthand a => Shorthand (DG12 a) where
  shorthand (DG12 (Glycerol r1 r2 _)) =
    "DG " <> shorthand r1 <> "/" <> shorthand r2 <> "/0:0"

instance Shorthand a => Shorthand (DG13 a) where
  shorthand (DG13 (Glycerol r1 _ r3)) =
    "DG " <> shorthand r1 <> "/0:0/" <> shorthand r3

instance Shorthand a => Shorthand (DG23 a) where
  shorthand (DG23 (Glycerol _ r2 r3)) =
    "DG 0:0/" <> shorthand r2 <> "/" <> shorthand r3

instance Shorthand a => Shorthand (MG1 a) where
  shorthand (MG1 (Glycerol r1 _ _)) =
    "MG " <> shorthand r1 <> "/0:0/0:0"

instance Shorthand a => Shorthand (MG2 a) where
  shorthand (MG2 (Glycerol _ r2 _)) =
    "MG 0:0/" <> shorthand r2 <> "/0:0"

instance Shorthand a => Shorthand (MG3 a) where
  shorthand (MG3 (Glycerol _ _ r3)) =
    "MG 0:0/0:0/" <> shorthand r3

instance NNomenclature a => NNomenclature (TG a) where
  nNomenclature (TG (Glycerol r1 r2 r3)) =
    "TG" <> nNomenclature r1 <> "/" <> nNomenclature r2 <> "/" <> nNomenclature r3

instance NNomenclature a => NNomenclature (DG12 a) where
  nNomenclature (DG12 (Glycerol r1 r2 _)) =
    "DG " <> nNomenclature r1 <> "/" <> nNomenclature r2 <> "/0:0"

instance NNomenclature a => NNomenclature (DG13 a) where
  nNomenclature (DG13 (Glycerol r1 _ r3)) =
    "DG " <> nNomenclature r1 <> "/0:0/" <> nNomenclature r3

instance NNomenclature a => NNomenclature (DG23 a) where
  nNomenclature (DG23 (Glycerol _ r2 r3)) =
    "DG 0:0/" <> nNomenclature r2 <> "/" <> nNomenclature r3

instance NNomenclature a => NNomenclature (MG1 a) where
  nNomenclature (MG1 (Glycerol r1 _ _)) =
    "MG " <> nNomenclature r1 <> "/0:0/0:0"

instance NNomenclature a => NNomenclature (MG2 a) where
  nNomenclature (MG2 (Glycerol _ r2 _)) =
    "MG 0:0/" <> nNomenclature r2 <> "/0:0"

instance NNomenclature a => NNomenclature (MG3 a) where
  nNomenclature (MG3 (Glycerol _ _ r3)) =
    "MG 0:0/0:0/" <> nNomenclature r3
