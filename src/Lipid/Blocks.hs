{-|
Module      : Lipid.Blocks
Description : Module contains lipid data types.
Copyright   : Michael Thomas
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}

module Lipid.Blocks where
--    (
--      Carbons(..)
--    , NumDoubleBonds(..)
--    , Position(..)
--    , Moiety(..)
--    , DoubleBond(..)
--    , MoietyData(..)
--    , Geometry(..)
--    , CarbonChain(..)
--    , TwoCombinedChains(..)
--    , ThreeCombinedChains(..)
--    , FourCombinedChains(..)
--    , Linkage(..)
--    , Radyl(..)
--    , TwoCombinedRadyls(..)
--    , ThreeCombinedRadyls(..)
--    , FourCombinedRadyls(..)
--    , SnPosition(..)
--    , PhosphatePosition(..)
--    , Shorthand(..)
--    , Nomenclature(..)
--    , doubleBondNumber
--    , doubleBondPositions
--    , doubleBondGeometries
--    , toDelta
--    , isBisAllylic
--    , dbToDelta
--    , toOmega
--    , dbToOmega
--    , listToString
--    , numberOfCarbonsAdd
 --) where

import Data.Maybe
import Data.Monoid ((<>))
import Data.List (sort, sortBy, group, maximumBy, intersperse, intercalate)
import Data.Ord (comparing, Down(..))
import Control.Arrow ((&&&))
import Control.Lens
import Lipid.Format


class Shorthand a where
  shorthand :: a -> String

class NNomenclature a where
  nNomenclature :: a -> String

instance Shorthand a => Shorthand (Maybe a) where
  shorthand Nothing = ""
  shorthand (Just x) = shorthand x

instance NNomenclature a => NNomenclature (Maybe a) where
  nNomenclature Nothing = ""
  nNomenclature (Just x) = nNomenclature x

class IsSaturated a where
  isSaturated :: a -> Bool

class IsMonounsaturated a where
  isMonounsaturated :: a -> Bool

class IsPolyunsaturated a where
  isPolyunsaturated :: a -> Bool

class IsBisAllylic a where
  isBisAllylic :: a -> Bool

-- |Carbons represents the number of carbons in a single carbon chain or
-- two or more combined chains.
newtype NumCarbons = NumCarbons { _getNumCarbons :: Integer }
                   deriving (Eq, Ord, Enum)

makeClassy ''NumCarbons

instance Show NumCarbons where
  show (NumCarbons n) = show n

instance Monoid NumCarbons where
  mempty = NumCarbons 0
  NumCarbons x `mappend` NumCarbons y = NumCarbons (x + y)

-- |NumDoubleBonds represents the number of carbon-carbon double bonds
-- on a carbon chain.
newtype NumDoubleBonds = NumDoubleBonds
  { _getNumDoubleBonds :: Integer
  } deriving (Eq, Ord, Enum)

makeClassy ''NumDoubleBonds

instance Show NumDoubleBonds where
  show (NumDoubleBonds n) = show n

instance Monoid NumDoubleBonds where
  mempty = NumDoubleBonds 0
  NumDoubleBonds x `mappend` NumDoubleBonds y = NumDoubleBonds (x + y)

-- |Position represents the position of carbon-carbon double bonds or
-- moieties on a carbon chain. Positions can be provided from the
-- methyl end (Omega) or from the linkage (Delta).
newtype OmegaPosition = OmegaPosition
  { _getOmegaPosition :: Integer
  } deriving (Show, Read, Eq, Ord, Enum, Num)

makeClassy ''OmegaPosition

instance NNomenclature OmegaPosition where
  nNomenclature (OmegaPosition x) = "n-" <> show x

newtype DeltaPosition = DeltaPosition
  { _getDeltaPosition :: Integer
  } deriving (Show, Read, Eq, Ord, Enum, Num)

makeClassy ''DeltaPosition

instance Shorthand DeltaPosition where
  shorthand (DeltaPosition x) = show x

-- |Geometry represent the geometry of carbon-carbon double bonds.
data Geometry
  = Cis
  | Trans
  deriving (Show, Read, Eq, Ord, Enum)

makePrisms ''Geometry

instance NNomenclature Geometry where
  nNomenclature _ = ""

instance Shorthand Geometry where
  shorthand Cis   = "Z"
  shorthand Trans = "E"

-- |Moiety represents moieties commonly found on carbon chains
data Moiety
  = Hydroxyl
  | Keto
  | Methyl
  deriving (Show, Read, Eq, Ord, Enum)

makePrisms ''Moiety

instance Shorthand Moiety where
   shorthand x =
     case x of
       Hydroxyl -> "OH"
       Keto     -> "O"
       Methyl   -> "Me"

-- |DoubleBond represents a carbon-carbon double bond as found on
-- carbon chains. A DoubleBond data type has two fields, Position
-- and Maybe Geometry.
data DoubleBond a = DoubleBond
  { _dbPosition :: a
  , _geometry   :: Maybe Geometry
  } deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable)

makeClassy ''DoubleBond

instance Shorthand a => Shorthand (DoubleBond a) where
  shorthand (DoubleBond p g) =
    shorthand p <> shorthand g

instance NNomenclature a => NNomenclature (DoubleBond a) where
  nNomenclature (DoubleBond p g) =
    nNomenclature p <> nNomenclature g

instance IsBisAllylic [DoubleBond DeltaPosition] where
  isBisAllylic dbs =
    case sort dbs of
      [x,y] ->
         x^.dbPosition.getDeltaPosition.to (+3) == y^.dbPosition.getDeltaPosition
      x:y:zs ->
        x^.dbPosition.getDeltaPosition.to (+3) /= y^.dbPosition.getDeltaPosition &&
        isBisAllylic (y:zs)
      _ -> False

instance IsBisAllylic [DoubleBond OmegaPosition] where
  isBisAllylic dbs =
    case sortBy (comparing Down) dbs of
      [x,y] ->
         x^.dbPosition.getOmegaPosition.to (\x -> x - 3) == y^.dbPosition.getOmegaPosition
      x:y:zs ->
        x^.dbPosition.getOmegaPosition.to (\x -> x - 3) /= y^.dbPosition.getOmegaPosition &&
        isBisAllylic (y:zs)
      _ -> False

instance IsBisAllylic [DoubleBond (Maybe DeltaPosition)] where
  isBisAllylic dbs =
    maybe False isBisAllylic $ traverse sequence dbs

instance IsBisAllylic [DoubleBond (Maybe OmegaPosition)] where
  isBisAllylic dbs =
    maybe False isBisAllylic $ traverse sequence dbs

-- |MoietyData represents a moiety on a carbon chain.
data MoietyData a = MoietyData
  { _moietyPosition :: a
  , _moiety         :: Moiety
  } deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable)

makeClassy ''MoietyData

instance Shorthand a => Shorthand (MoietyData a) where
  shorthand (MoietyData x y) =
    shorthand x <> shorthand y

renderCombinedChains x y f =
  show x <> ":" <> show (length y) <> wrapParen dbInfo
    where dbInfo = intercalate "," $ f <$> y

-- |CarbonChain represents a carbon chain.
data CarbonChain a b = CarbonChain
  { _carbons        :: NumCarbons
  , _moietyDataList :: [MoietyData a]
  , _doubleBonds    :: [DoubleBond b]
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeClassy ''CarbonChain

instance HasNumCarbons (CarbonChain a b) where
  numCarbons = carbons

instance Shorthand b => Shorthand (CarbonChain a b) where
  shorthand (CarbonChain x y z) =
    renderCombinedChains x z shorthand

instance NNomenclature b => NNomenclature (CarbonChain a b) where
  nNomenclature (CarbonChain x y z) =
    renderCombinedChains x z nNomenclature

instance IsBisAllylic [DoubleBond b] => IsBisAllylic (CarbonChain a b) where
  isBisAllylic cc =
    cc^.doubleBonds.to isBisAllylic

-- |CombinedChains represents combined carbon chains. For example,
-- a diradyl phosphatidylcholine can be written as PC 32:1.
data TwoCombinedChains a = TwoCombinedChains
   { _combinedNumCarbons     :: NumCarbons
   , _combinedNumDoubleBonds :: [[DoubleBond a]] }
   deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeClassy ''TwoCombinedChains

instance HasNumCarbons (TwoCombinedChains a) where
  numCarbons = combinedNumCarbons

instance Shorthand a => Shorthand (TwoCombinedChains a ) where
  shorthand (TwoCombinedChains x y) =
    renderCombinedChains x (concat y) shorthand

instance NNomenclature a => NNomenclature (TwoCombinedChains a) where
  nNomenclature (TwoCombinedChains x y) =
    renderCombinedChains x (concat y) nNomenclature

data ThreeCombinedChains a = ThreeCombinedChains
   { _threeCombinedNumCarbons     :: NumCarbons
   , _threeCombinedNumDoubleBonds :: [[DoubleBond a]] }
   deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeClassy ''ThreeCombinedChains

instance HasNumCarbons (ThreeCombinedChains a) where
  numCarbons = threeCombinedNumCarbons

instance Shorthand a => Shorthand (ThreeCombinedChains a ) where
  shorthand (ThreeCombinedChains x y) =
    renderCombinedChains x (concat y) shorthand

instance NNomenclature a => NNomenclature (ThreeCombinedChains a) where
  nNomenclature (ThreeCombinedChains x y) =
    renderCombinedChains x (concat y) nNomenclature

data Linkage
  = Acyl
  | Alkyl
  | Alkenyl
  deriving (Show, Read, Eq, Ord, Enum)

makePrisms ''Linkage

instance Shorthand Linkage where
  shorthand =
    \case
      Acyl    -> ""
      Alkyl   -> "O-"
      Alkenyl -> "P-"

data SnPosition
  = Sn1
  | Sn2
  | Sn3
  deriving (Show, Read, Eq, Ord, Enum)

makePrisms ''SnPosition

data Radyl a b = Radyl
   { _radylLinkage     :: Linkage
   , _radylCarbonChain :: CarbonChain a b }
   deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeClassy ''Radyl

instance (Shorthand a, Shorthand b) => Shorthand (Radyl a b) where
  shorthand (Radyl x y) =
    shorthand x <> shorthand y

instance (NNomenclature a, NNomenclature b) => NNomenclature (Radyl a b) where
  nNomenclature (Radyl x y) =
    shorthand x <> nNomenclature y

instance IsBisAllylic [DoubleBond b] => IsBisAllylic (Radyl a b) where
  isBisAllylic r =
    r^.radylCarbonChain.doubleBonds.to isBisAllylic

data TwoCombinedRadyls a = TwoCombinedRadyls
   { _radylLinkages           :: [Linkage]
   , _twoCombinedCarbonChains :: TwoCombinedChains a
   } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeClassy ''TwoCombinedRadyls

data ThreeCombinedRadyls a = ThreeCombinedRadyls
   { _threeRadylLinkages        :: [Linkage]
   , _threeCombinedCarbonChains :: ThreeCombinedChains a
   } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeClassy ''ThreeCombinedRadyls

data PhosphatePosition
  = P3'
  | P4'
  | P5'
  deriving (Show, Read, Eq, Ord, Enum)

makePrisms ''PhosphatePosition

instance Shorthand PhosphatePosition where
   shorthand =
     \case
       P3' -> "3'"
       P4' -> "4'"
       P5' -> "5'"

renderOmegaPositions :: [DoubleBond OmegaPosition] -> String
renderOmegaPositions dbs =
  if isBisAllylic dbs
    then nNomenclature (maximum dbs)
    else wrapParen $ intercalate "," (nNomenclature <$> dbs)

-- Helper functions are defined below. frequency function from a Stackoverflow reply.
frequency :: Ord a => [a] -> [(Int, a)]
frequency list = (length &&& head) <$> group (sort list)

linkageStrList :: [Linkage] -> [(Int, Linkage)]
linkageStrList = frequency . filter (/= Acyl)

freq2shorthand :: (Eq a, Num a) => (a, Linkage) -> String
freq2shorthand (x, y)
   | x == 1 = shorthand y
   | x == 2 = 'd' : shorthand y
   | x == 3 = 't' : shorthand y

links :: [Linkage] -> String
links x =
  intercalate "," $ freq2shorthand <$> linkageStrList x

numberOfDoubleBond :: CarbonChain a b -> NumDoubleBonds
numberOfDoubleBond c =
  c^.doubleBonds.to (NumDoubleBonds . fromIntegral . length)
