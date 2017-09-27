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
{-# LANGUAGE LambdaCase #-}

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
  charge _ = Just 1

data DG a
  = DG12 (Glycerol (Radyl a) (Radyl a) GlycerolHydroxyl)
  | DG13 (Glycerol (Radyl a) GlycerolHydroxyl (Radyl a))
  | DG23 (Glycerol GlycerolHydroxyl (Radyl a) (Radyl a))
  deriving (Show, Eq, Ord)

makePrisms ''DG

instance Functor DG where
  fmap f =
    \case
      DG12 (Glycerol r1 r2 GlycerolHydroxyl) ->
        DG12 $ Glycerol (f <$> r1) (f <$> r2) GlycerolHydroxyl
      DG13 (Glycerol r1 GlycerolHydroxyl r2) ->
        DG13 $ Glycerol (f <$> r1) GlycerolHydroxyl (f <$> r2)
      DG23 (Glycerol GlycerolHydroxyl r1 r2) ->
        DG23 $ Glycerol GlycerolHydroxyl (f <$> r1) (f <$> r2)


instance Foldable DG where
  foldMap f =
    \case
      DG12 (Glycerol r1 r2 _) ->
        foldMap f r1 <> foldMap f r2
      DG13 (Glycerol r1 _ r2) ->
        foldMap f r1 <> foldMap f r2
      DG23 (Glycerol _ r1 r2) ->
        foldMap f r1 <> foldMap f r2

instance Traversable DG where
  traverse f =
    \case
      DG12 (Glycerol r1 r2 _) ->
        (\x y -> DG12 (Glycerol x y GlycerolHydroxyl))
        <$> traverse f r1
        <*> traverse f r2
      DG13 (Glycerol r1 _ r2) ->
        (\x y -> DG13 (Glycerol x GlycerolHydroxyl y))
        <$> traverse f r1
        <*> traverse f r2
      DG23 (Glycerol _ r1 r2) ->
        (\x y -> DG23 (Glycerol GlycerolHydroxyl x y))
        <$> traverse f r1
        <*> traverse f r2

instance AllRadyls DG where
  allRadyls f =
    \case
      DG12 (Glycerol r1 r2 _) ->
        (\x y -> DG12 (Glycerol x y GlycerolHydroxyl))
        <$> f r1
        <*> f r2
      DG13 (Glycerol r1 _ r2) ->
        (\x y -> DG13 (Glycerol x GlycerolHydroxyl y))
        <$> f r1
        <*> f r2
      DG23 (Glycerol _ r1 r2) ->
        (\x y -> DG23 (Glycerol GlycerolHydroxyl x y))
        <$> f r1
        <*> f r2


instance ToElementalComposition (DG a) where
  toElementalComposition =
    \case
      DG12 g -> toElementalComposition g
      DG13 g -> toElementalComposition g
      DG23 g -> toElementalComposition g
  charge _ = Just 1



data MG a
  = MG1 (Glycerol (Radyl a) GlycerolHydroxyl GlycerolHydroxyl)
  | MG2 (Glycerol GlycerolHydroxyl (Radyl a) GlycerolHydroxyl)
  | MG3 (Glycerol GlycerolHydroxyl GlycerolHydroxyl (Radyl a))
  deriving (Show, Eq, Ord)

makePrisms ''MG

instance Functor MG where
  fmap f =
    \case
      MG1 (Glycerol r GlycerolHydroxyl GlycerolHydroxyl) ->
        MG1 $ Glycerol (f <$> r) GlycerolHydroxyl GlycerolHydroxyl
      MG2 (Glycerol GlycerolHydroxyl r GlycerolHydroxyl) ->
        MG2 $ Glycerol GlycerolHydroxyl (f <$> r) GlycerolHydroxyl
      MG3 (Glycerol GlycerolHydroxyl GlycerolHydroxyl r) ->
        MG3 $ Glycerol GlycerolHydroxyl GlycerolHydroxyl (f <$> r)

instance Foldable MG where
  foldMap f =
    \case
      MG1 (Glycerol r _ _) ->
        foldMap f r
      MG2 (Glycerol _ r _) ->
        foldMap f r
      MG3 (Glycerol _ _ r) ->
        foldMap f r

instance Traversable MG where
  traverse f =
    \case
      MG1 (Glycerol r _ _) ->
        (\x -> MG1 (Glycerol x GlycerolHydroxyl GlycerolHydroxyl)) <$> traverse f r
      MG2 (Glycerol _ r _) ->
        (\x -> MG2 (Glycerol GlycerolHydroxyl x GlycerolHydroxyl)) <$> traverse f r
      MG3 (Glycerol _ _ r) ->
        (MG3 . Glycerol GlycerolHydroxyl GlycerolHydroxyl) <$> traverse f r

instance AllRadyls MG where
  allRadyls f =
    \case
      MG1 (Glycerol r _ _) ->
        (\x -> MG1 (Glycerol x GlycerolHydroxyl GlycerolHydroxyl)) <$> f r
      MG2 (Glycerol _ r _) ->
        (\x -> MG2 (Glycerol GlycerolHydroxyl x GlycerolHydroxyl)) <$> f r
      MG3 (Glycerol _ _ r) ->
        (MG3 . Glycerol GlycerolHydroxyl GlycerolHydroxyl) <$> f r

instance ToElementalComposition (MG a) where
  toElementalComposition =
    \case
      MG1 g -> toElementalComposition g
      MG2 g -> toElementalComposition g
      MG3 g -> toElementalComposition g
  charge _ = Just 1

instance Shorthand a => Shorthand (TG a) where
  shorthand (TG (Glycerol r1 r2 r3)) =
    "TG " <> shorthand r1 <> "/" <> shorthand r2 <> "/" <> shorthand r3

instance Shorthand a => Shorthand (DG a) where
  shorthand =
    \case
      DG12 (Glycerol r1 r2 _) ->
        "DG " <> shorthand r1 <> "/" <> shorthand r2 <> "/0:0"
      DG13 (Glycerol r1 _ r3) ->
        "DG " <> shorthand r1 <> "/0:0/" <> shorthand r3
      DG23 (Glycerol _ r2 r3) ->
        "DG 0:0/" <> shorthand r2 <> "/" <> shorthand r3

instance Shorthand a => Shorthand (MG a) where
  shorthand =
    \case
      MG1 (Glycerol r1 _ _) ->
       "MG " <> shorthand r1 <> "/0:0/0:0"
      MG2 (Glycerol _ r2 _) ->
       "MG 0:0/" <> shorthand r2 <> "/0:0"
      MG3 (Glycerol _ _ r3) ->
       "MG 0:0/0:0/" <> shorthand r3

instance NNomenclature a => NNomenclature (TG a) where
  nNomenclature (TG (Glycerol r1 r2 r3)) =
    "TG" <> nNomenclature r1 <> "/" <> nNomenclature r2 <> "/" <> nNomenclature r3

instance NNomenclature a => NNomenclature (DG a) where
  nNomenclature =
    \case
      DG12 (Glycerol r1 r2 _) ->
        "DG " <> nNomenclature r1 <> "/" <> nNomenclature r2 <> "/0:0"
      DG13 (Glycerol r1 _ r3) ->
        "DG " <> nNomenclature r1 <> "/0:0/" <> nNomenclature r3
      DG23 (Glycerol _ r2 r3) ->
        "DG 0:0/" <> nNomenclature r2 <> "/" <> nNomenclature r3

instance NNomenclature a => NNomenclature (MG a) where
  nNomenclature =
    \case
      MG1 (Glycerol r1 _ _) ->
        "MG " <> nNomenclature r1 <> "/0:0/0:0"
      MG2 (Glycerol _ r2 _) ->
        "MG 0:0/" <> nNomenclature r2 <> "/0:0"
      MG3 (Glycerol _ _ r3) ->
        "MG 0:0/0:0/" <> nNomenclature r3
