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

instance HasGlycerol (PA a) PhosphatidicAcid (Radyl a) (Radyl a) where
  glycerol = getPA

newtype PC a = PC
  { _getPC :: Glycerol Phosphatidylcholine (Radyl a) (Radyl a)
  } deriving (Show, Eq, Ord)

makeLenses ''PC

instance Functor PC where
    fmap f (PC (Glycerol h r1 r2)) =
      PC $ Glycerol h (f <$> r1) (f <$> r2)

instance HasGlycerol (PC a) Phosphatidylcholine (Radyl a) (Radyl a) where
  glycerol = getPC

newtype PE a = PE
  { _getPE :: Glycerol Phosphatidylethanolamine (Radyl a) (Radyl a)
  } deriving (Show, Eq, Ord)

makeLenses ''PE

instance Functor PE where
    fmap f (PE (Glycerol h r1 r2)) =
      PE $ Glycerol h (f <$> r1) (f <$> r2)

instance HasGlycerol (PE a) Phosphatidylethanolamine (Radyl a) (Radyl a) where
  glycerol = getPE

newtype PG a = PG
  { _getPG :: Glycerol Phosphatidylglycerol (Radyl a) (Radyl a)
  } deriving (Show, Eq, Ord)

makeLenses ''PG

instance Functor PG where
    fmap f (PG (Glycerol h r1 r2)) =
      PG $ Glycerol h (f <$> r1) (f <$> r2)

instance HasGlycerol (PG a) Phosphatidylglycerol (Radyl a) (Radyl a) where
  glycerol = getPG

newtype PGP a = PGP
  { _getPGP :: Glycerol Phosphatidylgylcerolphosphate (Radyl a) (Radyl a)
  } deriving (Show, Eq, Ord)

makeLenses ''PGP

instance Functor PGP where
    fmap f (PGP (Glycerol h r1 r2)) =
      PGP $ Glycerol h (f <$> r1) (f <$> r2)

instance HasGlycerol (PGP a) Phosphatidylgylcerolphosphate (Radyl a) (Radyl a) where
  glycerol = getPGP

newtype PI a = PI
  { _getPI :: Glycerol Phosphatidylinositol (Radyl a) (Radyl a)
  } deriving (Show, Eq, Ord)

makeLenses ''PI

instance Functor PI where
    fmap f (PI (Glycerol h r1 r2)) =
      PI $ Glycerol h (f <$> r1) (f <$> r2)

instance HasGlycerol (PI a) Phosphatidylinositol (Radyl a) (Radyl a) where
  glycerol = getPI

newtype PIP a = PIP
  { _getPIP :: Glycerol PhosphatidylinositolMonophosphate (Radyl a) (Radyl a)
  } deriving (Show, Eq, Ord)

makeLenses ''PIP

instance Functor PIP where
    fmap f (PIP (Glycerol h r1 r2)) =
      PIP $ Glycerol h (f <$> r1) (f <$> r2)

instance HasGlycerol (PIP a) PhosphatidylinositolMonophosphate (Radyl a) (Radyl a) where
  glycerol = getPIP

newtype PIP2 a = PIP2
  { _getPIP2 :: Glycerol PhosphatidylinositolBisphosphate (Radyl a) (Radyl a)
  } deriving (Show, Eq, Ord)

makeLenses ''PIP2

instance Functor PIP2 where
    fmap f (PIP2 (Glycerol h r1 r2)) =
      PIP2 $ Glycerol h (f <$> r1) (f <$> r2)

instance HasGlycerol (PIP2 a) PhosphatidylinositolBisphosphate (Radyl a) (Radyl a) where
  glycerol = getPIP2

newtype PIP3 a = PIP3
  { _getPIP3 :: Glycerol PhosphatidylinositolTrisphosphate (Radyl a) (Radyl a)
  } deriving (Show, Eq, Ord)

makeLenses ''PIP3

instance Functor PIP3 where
    fmap f (PIP3 (Glycerol h r1 r2)) =
      PIP3 $ Glycerol h (f <$> r1) (f <$> r2)

instance HasGlycerol (PIP3 a) PhosphatidylinositolTrisphosphate (Radyl a) (Radyl a) where
  glycerol = getPIP3

newtype PS a = PS
  { _getPS :: Glycerol Phosphatidylserine (Radyl a) (Radyl a)
  } deriving (Show, Eq, Ord)

makeLenses ''PS

instance Functor PS where
    fmap f (PS (Glycerol h r1 r2)) =
      PS $ Glycerol h (f <$> r1) (f <$> r2)

instance HasGlycerol (PS a) Phosphatidylserine (Radyl a) (Radyl a) where
  glycerol = getPS
