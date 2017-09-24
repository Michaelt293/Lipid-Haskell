{-|
Module      : Lipid.KnownSn.Glycerophospholipid
Description : Glycerolipid data type and instances of Shorthand and
              NNomenclature defined.
Copyright   : Michael Thomas
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Lipid.KnownSn.Glycerophospholipid where

import Isotope
import Lipid.Blocks
import Lipid.Format
import Control.Lens
import Data.Monoid ((<>))


newtype PA a = PA
  { _getPA :: Glycerol (Radyl a) (Radyl a) PhosphatidicAcid
  } deriving (Show, Eq, Ord)

makeLenses ''PA

instance Functor PA where
    fmap f (PA (Glycerol r1 r2 h)) =
      PA $ Glycerol (f <$> r1) (f <$> r2) h

instance Foldable PA where
  foldMap f (PA (Glycerol r1 r2 _)) =
    foldMap f r1 <> foldMap f r2

instance Traversable PA where
  traverse f (PA (Glycerol r1 r2 h)) =
    (\x y -> PA (Glycerol x y h))
    <$> traverse f r1
    <*> traverse f r2

instance AllRadyls PA where
  allRadyls f (PA (Glycerol r1 r2 h)) =
    (\x y -> PA (Glycerol x y h))
    <$> f r1
    <*> f r2

instance Shorthand a => Shorthand (PA a) where
  shorthand (PA (Glycerol r1 r2 h)) =
    shorthand h <> " " <> shorthand r1 <> "/" <> shorthand r2

instance NNomenclature a => NNomenclature (PA a) where
  nNomenclature (PA (Glycerol r1 r2 h)) =
    shorthand h <> " " <> nNomenclature r1 <> "/" <> nNomenclature r2

instance HasGlycerol (PA a) (Radyl a) (Radyl a) PhosphatidicAcid where
  glycerol = getPA

instance ToElementalComposition (PA a) where
  toElementalComposition (PA g) = toElementalComposition g
  charge (PA g) = charge g

newtype PC a = PC
  { _getPC :: Glycerol (Radyl a) (Radyl a) Phosphatidylcholine
  } deriving (Show, Eq, Ord)

makeLenses ''PC

instance Functor PC where
    fmap f (PC (Glycerol r1 r2 h)) =
      PC $ Glycerol (f <$> r1) (f <$> r2) h

instance Foldable PC where
  foldMap f (PC (Glycerol r1 r2 _)) =
    foldMap f r1 <> foldMap f r2

instance Traversable PC where
  traverse f (PC (Glycerol r1 r2 h)) =
    (\x y -> PC (Glycerol x y h))
    <$> traverse f r1
    <*> traverse f r2

instance AllRadyls PC where
  allRadyls f (PC (Glycerol r1 r2 h)) =
    (\x y -> PC (Glycerol x y h))
    <$> f r1
    <*> f r2

instance Shorthand a => Shorthand (PC a) where
  shorthand (PC (Glycerol r1 r2 h)) =
    shorthand h <> " " <> shorthand r1 <> "/" <> shorthand r2

instance NNomenclature a => NNomenclature (PC a) where
  nNomenclature (PC (Glycerol r1 r2 h)) =
    shorthand h <> " " <> nNomenclature r1 <> "/" <> nNomenclature r2

instance HasGlycerol (PC a) (Radyl a) (Radyl a) Phosphatidylcholine where
  glycerol = getPC

instance ToElementalComposition (PC a) where
  toElementalComposition (PC g) = toElementalComposition g
  charge (PC g) = charge g

newtype PE a = PE
  { _getPE :: Glycerol (Radyl a) (Radyl a) Phosphatidylethanolamine
  } deriving (Show, Eq, Ord)

makeLenses ''PE

instance Functor PE where
    fmap f (PE (Glycerol r1 r2 h)) =
      PE $ Glycerol (f <$> r1) (f <$> r2) h

instance Foldable PE where
  foldMap f (PE (Glycerol r1 r2 _)) =
    foldMap f r1 <> foldMap f r2

instance Traversable PE where
  traverse f (PE (Glycerol r1 r2 h)) =
    (\x y -> PE (Glycerol x y h))
    <$> traverse f r1
    <*> traverse f r2

instance AllRadyls PE where
  allRadyls f (PE (Glycerol r1 r2 h)) =
    (\x y -> PE (Glycerol x y h))
    <$> f r1
    <*> f r2

instance Shorthand a => Shorthand (PE a) where
  shorthand (PE (Glycerol r1 r2 h)) =
    shorthand h <> " " <> shorthand r1 <> "/" <> shorthand r2

instance NNomenclature a => NNomenclature (PE a) where
  nNomenclature (PE (Glycerol r1 r2 h)) =
    shorthand h <> " " <> nNomenclature r1 <> "/" <> nNomenclature r2

instance HasGlycerol (PE a) (Radyl a) (Radyl a) Phosphatidylethanolamine where
  glycerol = getPE

instance ToElementalComposition (PE a) where
  toElementalComposition (PE g) = toElementalComposition g
  charge (PE g) = charge g

newtype PG a = PG
  { _getPG :: Glycerol (Radyl a) (Radyl a) Phosphatidylglycerol
  } deriving (Show, Eq, Ord)

makeLenses ''PG

instance Functor PG where
    fmap f (PG (Glycerol r1 r2 h)) =
      PG $ Glycerol (f <$> r1) (f <$> r2) h

instance Foldable PG where
  foldMap f (PG (Glycerol r1 r2 _)) =
    foldMap f r1 <> foldMap f r2

instance Traversable PG where
  traverse f (PG (Glycerol r1 r2 h)) =
    (\x y -> PG (Glycerol x y h))
    <$> traverse f r1
    <*> traverse f r2

instance AllRadyls PG where
  allRadyls f (PG (Glycerol r1 r2 h)) =
    (\x y -> PG (Glycerol x y h))
    <$> f r1
    <*> f r2

instance Shorthand a => Shorthand (PG a) where
  shorthand (PG (Glycerol r1 r2 h)) =
    shorthand h <> " " <> shorthand r1 <> "/" <> shorthand r2

instance NNomenclature a => NNomenclature (PG a) where
  nNomenclature (PG (Glycerol r1 r2 h)) =
    shorthand h <> " " <> nNomenclature r1 <> "/" <> nNomenclature r2

instance HasGlycerol (PG a) (Radyl a) (Radyl a) Phosphatidylglycerol where
  glycerol = getPG

instance ToElementalComposition (PG a) where
  toElementalComposition (PG g) = toElementalComposition g
  charge (PG g) = charge g

newtype PGP a = PGP
  { _getPGP :: Glycerol (Radyl a) (Radyl a) Phosphatidylgylcerolphosphate
  } deriving (Show, Eq, Ord)

makeLenses ''PGP

instance Functor PGP where
    fmap f (PGP (Glycerol r1 r2 h)) =
      PGP $ Glycerol (f <$> r1) (f <$> r2) h

instance Foldable PGP where
  foldMap f (PGP (Glycerol r1 r2 _)) =
    foldMap f r1 <> foldMap f r2

instance Traversable PGP where
  traverse f (PGP (Glycerol r1 r2 h)) =
    (\x y -> PGP (Glycerol x y h))
    <$> traverse f r1
    <*> traverse f r2

instance AllRadyls PGP where
  allRadyls f (PGP (Glycerol r1 r2 h)) =
    (\x y -> PGP (Glycerol x y h))
    <$> f r1
    <*> f r2

instance Shorthand a => Shorthand (PGP a) where
  shorthand (PGP (Glycerol r1 r2 h)) =
    shorthand h <> " " <> shorthand r1 <> "/" <> shorthand r2

instance NNomenclature a => NNomenclature (PGP a) where
  nNomenclature (PGP (Glycerol r1 r2 h)) =
    shorthand h <> " " <> nNomenclature r1 <> "/" <> nNomenclature r2

instance HasGlycerol (PGP a) (Radyl a) (Radyl a) Phosphatidylgylcerolphosphate where
  glycerol = getPGP

instance ToElementalComposition (PGP a) where
  toElementalComposition (PGP g) = toElementalComposition g
  charge (PGP g) = charge g

newtype PI a = PI
  { _getPI :: Glycerol (Radyl a) (Radyl a) Phosphatidylinositol
  } deriving (Show, Eq, Ord)

makeLenses ''PI

instance Functor PI where
    fmap f (PI (Glycerol r1 r2 h)) =
      PI $ Glycerol (f <$> r1) (f <$> r2) h

instance Foldable PI where
  foldMap f (PI (Glycerol r1 r2 _)) =
    foldMap f r1 <> foldMap f r2

instance Traversable PI where
  traverse f (PI (Glycerol r1 r2 h)) =
    (\x y -> PI (Glycerol x y h))
    <$> traverse f r1
    <*> traverse f r2

instance AllRadyls PI where
  allRadyls f (PI (Glycerol r1 r2 h)) =
    (\x y -> PI (Glycerol x y h))
    <$> f r1
    <*> f r2

instance Shorthand a => Shorthand (PI a) where
  shorthand (PI (Glycerol r1 r2 h)) =
    shorthand h <> " " <> shorthand r1 <> "/" <> shorthand r2

instance NNomenclature a => NNomenclature (PI a) where
  nNomenclature (PI (Glycerol r1 r2 h)) =
    shorthand h <> " " <> nNomenclature r1 <> "/" <> nNomenclature r2

instance HasGlycerol (PI a) (Radyl a) (Radyl a) Phosphatidylinositol where
  glycerol = getPI

instance ToElementalComposition (PI a) where
  toElementalComposition (PI g) = toElementalComposition g
  charge (PI g) = charge g

newtype PIP a = PIP
  { _getPIP :: Glycerol (Radyl a) (Radyl a) PhosphatidylinositolMonophosphate
  } deriving (Show, Eq, Ord)

makeLenses ''PIP

instance Functor PIP where
    fmap f (PIP (Glycerol r1 r2 h)) =
      PIP $ Glycerol (f <$> r1) (f <$> r2) h

instance Foldable PIP where
  foldMap f (PIP (Glycerol r1 r2 _)) =
    foldMap f r1 <> foldMap f r2

instance Traversable PIP where
  traverse f (PIP (Glycerol r1 r2 h)) =
    (\x y -> PIP (Glycerol x y h))
    <$> traverse f r1
    <*> traverse f r2

instance AllRadyls PIP where
  allRadyls f (PIP (Glycerol r1 r2 h)) =
    (\x y -> PIP (Glycerol x y h))
    <$> f r1
    <*> f r2

instance Shorthand a => Shorthand (PIP a) where
  shorthand (PIP (Glycerol r1 r2 h)) =
    shorthand h <> " " <> shorthand r1 <> "/" <> shorthand r2

instance NNomenclature a => NNomenclature (PIP a) where
  nNomenclature (PIP (Glycerol r1 r2 h)) =
    shorthand h <> " " <> nNomenclature r1 <> "/" <> nNomenclature r2

instance HasGlycerol (PIP a) (Radyl a) (Radyl a) PhosphatidylinositolMonophosphate where
  glycerol = getPIP

instance ToElementalComposition (PIP a) where
  toElementalComposition (PIP g) = toElementalComposition g
  charge (PIP g) = charge g

newtype PIP2 a = PIP2
  { _getPIP2 :: Glycerol (Radyl a) (Radyl a) PhosphatidylinositolBisphosphate
  } deriving (Show, Eq, Ord)

makeLenses ''PIP2

instance Functor PIP2 where
    fmap f (PIP2 (Glycerol r1 r2 h)) =
      PIP2 $ Glycerol (f <$> r1) (f <$> r2) h

instance Foldable PIP2 where
  foldMap f (PIP2 (Glycerol r1 r2 _)) =
    foldMap f r1 <> foldMap f r2

instance Traversable PIP2 where
  traverse f (PIP2 (Glycerol r1 r2 h)) =
    (\x y -> PIP2 (Glycerol x y h))
    <$> traverse f r1
    <*> traverse f r2

instance AllRadyls PIP2 where
  allRadyls f (PIP2 (Glycerol r1 r2 h)) =
    (\x y -> PIP2 (Glycerol x y h))
    <$> f r1
    <*> f r2

instance Shorthand a => Shorthand (PIP2 a) where
  shorthand (PIP2 (Glycerol r1 r2 h)) =
    shorthand h <> " " <> shorthand r1 <> "/" <> shorthand r2

instance NNomenclature a => NNomenclature (PIP2 a) where
  nNomenclature (PIP2 (Glycerol r1 r2 h)) =
    shorthand h <> " " <> nNomenclature r1 <> "/" <> nNomenclature r2

instance HasGlycerol (PIP2 a) (Radyl a) (Radyl a) PhosphatidylinositolBisphosphate where
  glycerol = getPIP2

instance ToElementalComposition (PIP2 a) where
  toElementalComposition (PIP2 g) = toElementalComposition g
  charge (PIP2 g) = charge g

newtype PIP3 a = PIP3
  { _getPIP3 :: Glycerol (Radyl a) (Radyl a) PhosphatidylinositolTrisphosphate
  } deriving (Show, Eq, Ord)

makeLenses ''PIP3

instance Functor PIP3 where
    fmap f (PIP3 (Glycerol r1 r2 h)) =
      PIP3 $ Glycerol (f <$> r1) (f <$> r2) h

instance Foldable PIP3 where
  foldMap f (PIP3 (Glycerol r1 r2 _)) =
    foldMap f r1 <> foldMap f r2

instance Traversable PIP3 where
  traverse f (PIP3 (Glycerol r1 r2 h)) =
    (\x y -> PIP3 (Glycerol x y h))
    <$> traverse f r1
    <*> traverse f r2

instance AllRadyls PIP3 where
  allRadyls f (PIP3 (Glycerol r1 r2 h)) =
    (\x y -> PIP3 (Glycerol x y h))
    <$> f r1
    <*> f r2

instance Shorthand a => Shorthand (PIP3 a) where
  shorthand (PIP3 (Glycerol r1 r2 h)) =
    shorthand h <> " " <> shorthand r1 <> "/" <> shorthand r2

instance NNomenclature a => NNomenclature (PIP3 a) where
  nNomenclature (PIP3 (Glycerol r1 r2 h)) =
    shorthand h <> " " <> nNomenclature r1 <> "/" <> nNomenclature r2

instance HasGlycerol (PIP3 a) (Radyl a) (Radyl a) PhosphatidylinositolTrisphosphate where
  glycerol = getPIP3

instance ToElementalComposition (PIP3 a) where
  toElementalComposition (PIP3 g) = toElementalComposition g
  charge (PIP3 g) = charge g

newtype PS a = PS
  { _getPS :: Glycerol (Radyl a) (Radyl a) Phosphatidylserine
  } deriving (Show, Eq, Ord)

makeLenses ''PS

instance Functor PS where
    fmap f (PS (Glycerol r1 r2 h)) =
      PS $ Glycerol (f <$> r1) (f <$> r2) h

instance Foldable PS where
  foldMap f (PS (Glycerol r1 r2 _)) =
    foldMap f r1 <> foldMap f r2

instance Traversable PS where
  traverse f (PS (Glycerol r1 r2 h)) =
    (\x y -> PS (Glycerol x y h))
    <$> traverse f r1
    <*> traverse f r2

instance AllRadyls PS where
  allRadyls f (PS (Glycerol r1 r2 h)) =
    (\x y -> PS (Glycerol x y h))
    <$> f r1
    <*> f r2

instance Shorthand a => Shorthand (PS a) where
  shorthand (PS (Glycerol r1 r2 h)) =
    shorthand h <> " " <> shorthand r1 <> "/" <> shorthand r2

instance NNomenclature a => NNomenclature (PS a) where
  nNomenclature (PS (Glycerol r1 r2 h)) =
    shorthand h <> " " <> nNomenclature r1 <> "/" <> nNomenclature r2

instance HasGlycerol (PS a) (Radyl a) (Radyl a) Phosphatidylserine where
  glycerol = getPS

instance ToElementalComposition (PS a) where
  toElementalComposition (PS g) = toElementalComposition g
  charge (PS g) = charge g
