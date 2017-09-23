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

import Lipid.Blocks
import Lipid.Format
import Control.Lens
import Data.Monoid ((<>))


newtype PA a = PA
  { _getPA :: Glycerol PhosphatidicAcid (Radyl a) (Radyl a)
  } deriving (Show, Eq, Ord)

makeLenses ''PA

instance Functor PA where
    fmap f (PA (Glycerol h r1 r2)) =
      PA $ Glycerol h (f <$> r1) (f <$> r2)

instance Foldable PA where
  foldMap f (PA (Glycerol _ r1 r2)) =
    foldMap f r1 <> foldMap f r2

instance Traversable PA where
  traverse f (PA (Glycerol h r1 r2)) =
    (\x y -> PA (Glycerol h x y))
    <$> traverse f r1
    <*> traverse f r2

instance AllRadyls PA where
  allRadyls f (PA (Glycerol h r1 r2)) =
    (\x y -> PA (Glycerol h x y))
    <$> f r1
    <*> f r2

instance Shorthand a => Shorthand (PA a) where
  shorthand (PA (Glycerol h r1 r2)) =
    shorthand h <> " " <> shorthand r1 <> "/" <> shorthand r2

instance NNomenclature a => NNomenclature (PA a) where
  nNomenclature (PA (Glycerol h r1 r2)) =
    shorthand h <> " " <> nNomenclature r1 <> "/" <> nNomenclature r2

instance HasGlycerol (PA a) PhosphatidicAcid (Radyl a) (Radyl a) where
  glycerol = getPA

newtype PC a = PC
  { _getPC :: Glycerol Phosphatidylcholine (Radyl a) (Radyl a)
  } deriving (Show, Eq, Ord)

makeLenses ''PC

instance Functor PC where
    fmap f (PC (Glycerol h r1 r2)) =
      PC $ Glycerol h (f <$> r1) (f <$> r2)

instance Foldable PC where
  foldMap f (PC (Glycerol _ r1 r2)) =
    foldMap f r1 <> foldMap f r2

instance Traversable PC where
  traverse f (PC (Glycerol h r1 r2)) =
    (\x y -> PC (Glycerol h x y))
    <$> traverse f r1
    <*> traverse f r2

instance AllRadyls PC where
  allRadyls f (PC (Glycerol h r1 r2)) =
    (\x y -> PC (Glycerol h x y))
    <$> f r1
    <*> f r2

instance Shorthand a => Shorthand (PC a) where
  shorthand (PC (Glycerol h r1 r2)) =
    shorthand h <> " " <> shorthand r1 <> "/" <> shorthand r2

instance NNomenclature a => NNomenclature (PC a) where
  nNomenclature (PC (Glycerol h r1 r2)) =
    shorthand h <> " " <> nNomenclature r1 <> "/" <> nNomenclature r2

instance HasGlycerol (PC a) Phosphatidylcholine (Radyl a) (Radyl a) where
  glycerol = getPC

newtype PE a = PE
  { _getPE :: Glycerol Phosphatidylethanolamine (Radyl a) (Radyl a)
  } deriving (Show, Eq, Ord)

makeLenses ''PE

instance Functor PE where
    fmap f (PE (Glycerol h r1 r2)) =
      PE $ Glycerol h (f <$> r1) (f <$> r2)

instance Foldable PE where
  foldMap f (PE (Glycerol _ r1 r2)) =
    foldMap f r1 <> foldMap f r2

instance Traversable PE where
  traverse f (PE (Glycerol h r1 r2)) =
    (\x y -> PE (Glycerol h x y))
    <$> traverse f r1
    <*> traverse f r2

instance AllRadyls PE where
  allRadyls f (PE (Glycerol h r1 r2)) =
    (\x y -> PE (Glycerol h x y))
    <$> f r1
    <*> f r2

instance Shorthand a => Shorthand (PE a) where
  shorthand (PE (Glycerol h r1 r2)) =
    shorthand h <> " " <> shorthand r1 <> "/" <> shorthand r2

instance NNomenclature a => NNomenclature (PE a) where
  nNomenclature (PE (Glycerol h r1 r2)) =
    shorthand h <> " " <> nNomenclature r1 <> "/" <> nNomenclature r2

instance HasGlycerol (PE a) Phosphatidylethanolamine (Radyl a) (Radyl a) where
  glycerol = getPE

newtype PG a = PG
  { _getPG :: Glycerol Phosphatidylglycerol (Radyl a) (Radyl a)
  } deriving (Show, Eq, Ord)

makeLenses ''PG

instance Functor PG where
    fmap f (PG (Glycerol h r1 r2)) =
      PG $ Glycerol h (f <$> r1) (f <$> r2)

instance Foldable PG where
  foldMap f (PG (Glycerol _ r1 r2)) =
    foldMap f r1 <> foldMap f r2

instance Traversable PG where
  traverse f (PG (Glycerol h r1 r2)) =
    (\x y -> PG (Glycerol h x y))
    <$> traverse f r1
    <*> traverse f r2

instance AllRadyls PG where
  allRadyls f (PG (Glycerol h r1 r2)) =
    (\x y -> PG (Glycerol h x y))
    <$> f r1
    <*> f r2

instance Shorthand a => Shorthand (PG a) where
  shorthand (PG (Glycerol h r1 r2)) =
    shorthand h <> " " <> shorthand r1 <> "/" <> shorthand r2

instance NNomenclature a => NNomenclature (PG a) where
  nNomenclature (PG (Glycerol h r1 r2)) =
    shorthand h <> " " <> nNomenclature r1 <> "/" <> nNomenclature r2

instance HasGlycerol (PG a) Phosphatidylglycerol (Radyl a) (Radyl a) where
  glycerol = getPG

newtype PGP a = PGP
  { _getPGP :: Glycerol Phosphatidylgylcerolphosphate (Radyl a) (Radyl a)
  } deriving (Show, Eq, Ord)

makeLenses ''PGP

instance Functor PGP where
    fmap f (PGP (Glycerol h r1 r2)) =
      PGP $ Glycerol h (f <$> r1) (f <$> r2)

instance Foldable PGP where
  foldMap f (PGP (Glycerol _ r1 r2)) =
    foldMap f r1 <> foldMap f r2

instance Traversable PGP where
  traverse f (PGP (Glycerol h r1 r2)) =
    (\x y -> PGP (Glycerol h x y))
    <$> traverse f r1
    <*> traverse f r2

instance AllRadyls PGP where
  allRadyls f (PGP (Glycerol h r1 r2)) =
    (\x y -> PGP (Glycerol h x y))
    <$> f r1
    <*> f r2

instance Shorthand a => Shorthand (PGP a) where
  shorthand (PGP (Glycerol h r1 r2)) =
    shorthand h <> " " <> shorthand r1 <> "/" <> shorthand r2

instance NNomenclature a => NNomenclature (PGP a) where
  nNomenclature (PGP (Glycerol h r1 r2)) =
    shorthand h <> " " <> nNomenclature r1 <> "/" <> nNomenclature r2

instance HasGlycerol (PGP a) Phosphatidylgylcerolphosphate (Radyl a) (Radyl a) where
  glycerol = getPGP

newtype PI a = PI
  { _getPI :: Glycerol Phosphatidylinositol (Radyl a) (Radyl a)
  } deriving (Show, Eq, Ord)

makeLenses ''PI

instance Functor PI where
    fmap f (PI (Glycerol h r1 r2)) =
      PI $ Glycerol h (f <$> r1) (f <$> r2)

instance Foldable PI where
  foldMap f (PI (Glycerol _ r1 r2)) =
    foldMap f r1 <> foldMap f r2

instance Traversable PI where
  traverse f (PI (Glycerol h r1 r2)) =
    (\x y -> PI (Glycerol h x y))
    <$> traverse f r1
    <*> traverse f r2

instance AllRadyls PI where
  allRadyls f (PI (Glycerol h r1 r2)) =
    (\x y -> PI (Glycerol h x y))
    <$> f r1
    <*> f r2

instance Shorthand a => Shorthand (PI a) where
  shorthand (PI (Glycerol h r1 r2)) =
    shorthand h <> " " <> shorthand r1 <> "/" <> shorthand r2

instance NNomenclature a => NNomenclature (PI a) where
  nNomenclature (PI (Glycerol h r1 r2)) =
    shorthand h <> " " <> nNomenclature r1 <> "/" <> nNomenclature r2

instance HasGlycerol (PI a) Phosphatidylinositol (Radyl a) (Radyl a) where
  glycerol = getPI

newtype PIP a = PIP
  { _getPIP :: Glycerol PhosphatidylinositolMonophosphate (Radyl a) (Radyl a)
  } deriving (Show, Eq, Ord)

makeLenses ''PIP

instance Functor PIP where
    fmap f (PIP (Glycerol h r1 r2)) =
      PIP $ Glycerol h (f <$> r1) (f <$> r2)

instance Foldable PIP where
  foldMap f (PIP (Glycerol _ r1 r2)) =
    foldMap f r1 <> foldMap f r2

instance Traversable PIP where
  traverse f (PIP (Glycerol h r1 r2)) =
    (\x y -> PIP (Glycerol h x y))
    <$> traverse f r1
    <*> traverse f r2

instance AllRadyls PIP where
  allRadyls f (PIP (Glycerol h r1 r2)) =
    (\x y -> PIP (Glycerol h x y))
    <$> f r1
    <*> f r2

instance Shorthand a => Shorthand (PIP a) where
  shorthand (PIP (Glycerol h r1 r2)) =
    shorthand h <> " " <> shorthand r1 <> "/" <> shorthand r2

instance NNomenclature a => NNomenclature (PIP a) where
  nNomenclature (PIP (Glycerol h r1 r2)) =
    shorthand h <> " " <> nNomenclature r1 <> "/" <> nNomenclature r2

instance HasGlycerol (PIP a) PhosphatidylinositolMonophosphate (Radyl a) (Radyl a) where
  glycerol = getPIP

newtype PIP2 a = PIP2
  { _getPIP2 :: Glycerol PhosphatidylinositolBisphosphate (Radyl a) (Radyl a)
  } deriving (Show, Eq, Ord)

makeLenses ''PIP2

instance Functor PIP2 where
    fmap f (PIP2 (Glycerol h r1 r2)) =
      PIP2 $ Glycerol h (f <$> r1) (f <$> r2)

instance Foldable PIP2 where
  foldMap f (PIP2 (Glycerol _ r1 r2)) =
    foldMap f r1 <> foldMap f r2

instance Traversable PIP2 where
  traverse f (PIP2 (Glycerol h r1 r2)) =
    (\x y -> PIP2 (Glycerol h x y))
    <$> traverse f r1
    <*> traverse f r2

instance AllRadyls PIP2 where
  allRadyls f (PIP2 (Glycerol h r1 r2)) =
    (\x y -> PIP2 (Glycerol h x y))
    <$> f r1
    <*> f r2

instance Shorthand a => Shorthand (PIP2 a) where
  shorthand (PIP2 (Glycerol h r1 r2)) =
    shorthand h <> " " <> shorthand r1 <> "/" <> shorthand r2

instance NNomenclature a => NNomenclature (PIP2 a) where
  nNomenclature (PIP2 (Glycerol h r1 r2)) =
    shorthand h <> " " <> nNomenclature r1 <> "/" <> nNomenclature r2

instance HasGlycerol (PIP2 a) PhosphatidylinositolBisphosphate (Radyl a) (Radyl a) where
  glycerol = getPIP2

newtype PIP3 a = PIP3
  { _getPIP3 :: Glycerol PhosphatidylinositolTrisphosphate (Radyl a) (Radyl a)
  } deriving (Show, Eq, Ord)

makeLenses ''PIP3

instance Functor PIP3 where
    fmap f (PIP3 (Glycerol h r1 r2)) =
      PIP3 $ Glycerol h (f <$> r1) (f <$> r2)

instance Foldable PIP3 where
  foldMap f (PIP3 (Glycerol _ r1 r2)) =
    foldMap f r1 <> foldMap f r2

instance Traversable PIP3 where
  traverse f (PIP3 (Glycerol h r1 r2)) =
    (\x y -> PIP3 (Glycerol h x y))
    <$> traverse f r1
    <*> traverse f r2

instance AllRadyls PIP3 where
  allRadyls f (PIP3 (Glycerol h r1 r2)) =
    (\x y -> PIP3 (Glycerol h x y))
    <$> f r1
    <*> f r2

instance Shorthand a => Shorthand (PIP3 a) where
  shorthand (PIP3 (Glycerol h r1 r2)) =
    shorthand h <> " " <> shorthand r1 <> "/" <> shorthand r2

instance NNomenclature a => NNomenclature (PIP3 a) where
  nNomenclature (PIP3 (Glycerol h r1 r2)) =
    shorthand h <> " " <> nNomenclature r1 <> "/" <> nNomenclature r2

instance HasGlycerol (PIP3 a) PhosphatidylinositolTrisphosphate (Radyl a) (Radyl a) where
  glycerol = getPIP3

newtype PS a = PS
  { _getPS :: Glycerol Phosphatidylserine (Radyl a) (Radyl a)
  } deriving (Show, Eq, Ord)

makeLenses ''PS

instance Functor PS where
    fmap f (PS (Glycerol h r1 r2)) =
      PS $ Glycerol h (f <$> r1) (f <$> r2)

instance Foldable PS where
  foldMap f (PS (Glycerol _ r1 r2)) =
    foldMap f r1 <> foldMap f r2

instance Traversable PS where
  traverse f (PS (Glycerol h r1 r2)) =
    (\x y -> PS (Glycerol h x y))
    <$> traverse f r1
    <*> traverse f r2

instance AllRadyls PS where
  allRadyls f (PS (Glycerol h r1 r2)) =
    (\x y -> PS (Glycerol h x y))
    <$> f r1
    <*> f r2

instance Shorthand a => Shorthand (PS a) where
  shorthand (PS (Glycerol h r1 r2)) =
    shorthand h <> " " <> shorthand r1 <> "/" <> shorthand r2

instance NNomenclature a => NNomenclature (PS a) where
  nNomenclature (PS (Glycerol h r1 r2)) =
    shorthand h <> " " <> nNomenclature r1 <> "/" <> nNomenclature r2

instance HasGlycerol (PS a) Phosphatidylserine (Radyl a) (Radyl a) where
  glycerol = getPS
