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
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Lipid.KnownSn.Glycerolipid where

import Lipid.Blocks
import Lipid.Format
import Control.Lens
import Data.Monoid ((<>))
import Data.List (sort)

newtype TG a = TG
  { _getTG :: Glycerol (Radyl a) (Radyl a) (Radyl a)
  } deriving (Show, Eq, Ord)

makeLenses ''TG

instance Functor TG where
    fmap f (TG (Glycerol r1 r2 r3)) =
      TG $ Glycerol (f <$> r1) (f <$> r2) (f <$> r3)

instance HasGlycerol (TG a) (Radyl a) (Radyl a) (Radyl a) where
  glycerol = getTG

newtype DG12 a = DG12
  { _getDG12 :: Glycerol (Radyl a) (Radyl a) ()
  } deriving (Show, Eq, Ord)

makeLenses ''DG12

instance Functor DG12 where
    fmap f (DG12 (Glycerol r1 r2 ())) =
      DG12 $ Glycerol (f <$> r1) (f <$> r2) ()

instance HasGlycerol (DG12 a) (Radyl a) (Radyl a) () where
  glycerol = getDG12

newtype DG13 a = DG13
  { _getDG13 :: Glycerol (Radyl a) () (Radyl a)
  } deriving (Show, Eq, Ord)

makeLenses ''DG13

instance Functor DG13 where
  fmap f (DG13 (Glycerol r1 () r2)) =
    DG13 $ Glycerol (f <$> r1) () (f <$> r2)

instance HasGlycerol (DG13 a) (Radyl a) () (Radyl a) where
  glycerol = getDG13

newtype DG23 a = DG23
  { _getDG23 :: Glycerol () (Radyl a) (Radyl a)
  } deriving (Show, Eq, Ord)

makeLenses ''DG23

instance Functor DG23 where
  fmap f (DG23 (Glycerol () r1 r2)) =
    DG23 $ Glycerol () (f <$> r1) (f <$> r2)

instance HasGlycerol (DG23 a) () (Radyl a) (Radyl a) where
  glycerol = getDG23

newtype MG1 a = MG1
  { _getMG1 :: Glycerol (Radyl a) () ()
  } deriving (Show, Eq, Ord)

makeLenses ''MG1

instance Functor MG1 where
  fmap f (MG1 (Glycerol r () ())) =
    MG1 $ Glycerol (f <$> r) () ()

instance HasGlycerol (MG1 a) (Radyl a) () () where
  glycerol = getMG1

newtype MG2 a = MG2
  { _getMG2 :: Glycerol () (Radyl a) ()
  } deriving (Show, Eq, Ord)

makeLenses ''MG2

instance Functor MG2 where
  fmap f (MG2 (Glycerol () r ())) =
    MG2 $ Glycerol () (f <$> r) ()

instance HasGlycerol (MG2 a) () (Radyl a) () where
  glycerol = getMG2

newtype MG3 a = MG3
  { _getMG3 :: Glycerol () () (Radyl a)
  } deriving (Show, Eq, Ord)

makeLenses ''MG3

instance Functor MG3 where
  fmap f (MG3 (Glycerol () () r)) =
    MG3 $ Glycerol () () (f <$> r)

instance HasGlycerol (MG3 a) () () (Radyl a) where
  glycerol = getMG3

instance Shorthand a => Shorthand (TG a) where
  shorthand (TG (Glycerol r1 r2 r3)) =
    shorthand r1 <> "/" <> shorthand r2 <> "/" <> shorthand r3

instance Shorthand a => Shorthand (DG12 a) where
  shorthand (DG12 (Glycerol r1 r2 _)) =
    shorthand r1 <> "/" <> shorthand r2 <> "/0:0"

instance Shorthand a => Shorthand (DG13 a) where
  shorthand (DG13 (Glycerol r1 _ r3)) =
    shorthand r1 <> "/0:0/" <> shorthand r3

instance Shorthand a => Shorthand (DG23 a) where
  shorthand (DG23 (Glycerol _ r2 r3)) =
    "0:0/" <> shorthand r2 <> "/" <> shorthand r3

instance Shorthand a => Shorthand (MG1 a) where
  shorthand (MG1 (Glycerol r1 _ _)) =
    shorthand r1 <> "/0:0/0:0"

instance Shorthand a => Shorthand (MG2 a) where
  shorthand (MG2 (Glycerol _ r2 _)) =
    "0:0/" <> shorthand r2 <> "/0:0"

instance Shorthand a => Shorthand (MG3 a) where
  shorthand (MG3 (Glycerol _ _ r3)) =
    "0:0/0:0/" <> shorthand r3

instance NNomenclature a => NNomenclature (TG a) where
  nNomenclature (TG (Glycerol r1 r2 r3)) =
    nNomenclature r1 <> "/" <> nNomenclature r2 <> "/" <> nNomenclature r3

instance NNomenclature a => NNomenclature (DG12 a) where
  nNomenclature (DG12 (Glycerol r1 r2 _)) =
    nNomenclature r1 <> "/" <> nNomenclature r2 <> "/0:0"

instance NNomenclature a => NNomenclature (DG13 a) where
  nNomenclature (DG13 (Glycerol r1 _ r3)) =
    nNomenclature r1 <> "/0:0/" <> nNomenclature r3

instance NNomenclature a => NNomenclature (DG23 a) where
  nNomenclature (DG23 (Glycerol _ r2 r3)) =
    "0:0/" <> nNomenclature r2 <> "/" <> nNomenclature r3

instance NNomenclature a => NNomenclature (MG1 a) where
  nNomenclature (MG1 (Glycerol r1 _ _)) =
    nNomenclature r1 <> "/0:0/0:0"

instance NNomenclature a => NNomenclature (MG2 a) where
  nNomenclature (MG2 (Glycerol _ r2 _)) =
    "0:0/" <> nNomenclature r2 <> "/0:0"

instance NNomenclature a => NNomenclature (MG3 a) where
  nNomenclature (MG3 (Glycerol _ _ r3)) =
    "0:0/0:0/" <> nNomenclature r3
