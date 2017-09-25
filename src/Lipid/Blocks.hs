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

import Isotope
import Data.Monoid ((<>))
import Data.List (sort, group, intercalate)
import Data.Ord (comparing, Down(..))
import Data.Maybe (fromMaybe)
import Data.Bifunctor
import Control.Arrow ((&&&))
import Control.Lens
import Lipid.Format


class Shorthand a where
  shorthand :: a -> String

class NNomenclature a where
  nNomenclature :: a -> String

class IsSaturated a where
  isSaturated :: a -> Maybe Bool

class IsMonounsaturated a where
  isMonounsaturated :: a -> Maybe Bool

class IsPolyunsaturated a where
  isPolyunsaturated :: a -> Maybe Bool

class IsBisAllylic a where
  isBisAllylic :: a -> Maybe Bool

-- |Carbons represents the number of carbons in a single carbon chain or
-- two or more combined chains.
newtype NumCarbons = NumCarbons { _getNumCarbons :: Integer }
                   deriving (Eq, Ord, Enum, Num, Real, Integral)

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

class Position a where
  getPosition :: a -> Maybe Integer
  getPositions :: [a] -> Maybe [Integer]
  getPositions x = sort <$> traverse getPosition x

-- |Position represents the position of carbon-carbon double bonds or
-- moieties on a carbon chain. Positions can be provided from the
-- methyl end (Omega) or from the linkage (Delta).
newtype OmegaPosition = OmegaPosition
  { _getOmegaPosition :: Integer
  } deriving (Show, Read, Eq, Ord, Enum, Num)

makeClassy ''OmegaPosition

instance NNomenclature (Maybe OmegaPosition) where
  nNomenclature Nothing = "?"
  nNomenclature (Just x) = nNomenclature x

instance NNomenclature OmegaPosition where
  nNomenclature (OmegaPosition x) = "n-" <> show x

instance Position OmegaPosition where
  getPosition = Just . _getOmegaPosition

instance Position a => Position (Maybe a) where
  getPosition p = getPosition =<< p

newtype DeltaPosition = DeltaPosition
  { _getDeltaPosition :: Integer
  } deriving (Show, Read, Eq, Ord, Enum, Num)

makeClassy ''DeltaPosition

instance Shorthand DeltaPosition where
  shorthand (DeltaPosition x) = show x

instance Shorthand (Maybe DeltaPosition) where
  shorthand Nothing = "?"
  shorthand (Just x) = shorthand x

instance Position DeltaPosition where
  getPosition = Just . _getDeltaPosition

-- |Geometry represent the geometry of carbon-carbon double bonds.
data Geometry
  = Cis
  | Trans
  deriving (Show, Read, Eq, Ord, Enum)

makePrisms ''Geometry

-- instance NNomenclature Geometry where
--   nNomenclature _ = ""

instance Shorthand Geometry where
  shorthand Cis   = "Z"
  shorthand Trans = "E"

instance Shorthand (Maybe Geometry) where
  shorthand Nothing = ""
  shorthand (Just x) = shorthand x

-- |Moiety represents moieties commonly found on carbon chains
-- data Moiety
--   = Hydroxyl
--   | Keto
--   | Methyl
--   deriving (Show, Read, Eq, Ord, Enum)
--
-- makePrisms ''Moiety
--
-- instance Shorthand Moiety where
--    shorthand x =
--      case x of
--        Hydroxyl -> "OH"
--        Keto     -> "O"
--        Methyl   -> "Me"

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
  nNomenclature (DoubleBond p _) =
    nNomenclature p

instance Position a => Position (DoubleBond a) where
  getPosition (DoubleBond p _) = getPosition p

instance Position a => IsBisAllylic [DoubleBond a] where
  isBisAllylic dbs = go <$> getPositions dbs
    where
      go ps =
        case ps of
          [x,y] -> x + 3 == y
          x:y:zs -> x + 3 == y && go (y:zs)
          _ -> False

-- instance IsBisAllylic [DoubleBond OmegaPosition] where
--   isBisAllylic dbs =
--     case sort dbs of
--       [x,y] ->
--          Just $ x^.dbPosition.getOmegaPosition.to (+3) == y^.dbPosition.getOmegaPosition
--       x:y:zs ->
--         liftA2
--           (&&)
--           (pure (x^.dbPosition.getOmegaPosition.to (+3) == y^.dbPosition.getOmegaPosition))
--           (isBisAllylic (y:zs))
--       _ -> Just False

-- instance Position a => IsBisAllylic [DoubleBond (Maybe a)] where
--   isBisAllylic dbs =
--     isBisAllylic =<< traverse sequence dbs

-- instance IsBisAllylic [DoubleBond (Maybe OmegaPosition)] where
--   isBisAllylic dbs =
--     isBisAllylic =<< traverse sequence dbs

-- |MoietyData represents a moiety on a carbon chain.
-- data MoietyData a = MoietyData
--   { _moietyPosition :: a
--   , _moiety         :: Moiety
--   } deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable)
--
-- makeClassy ''MoietyData
--
-- instance Shorthand a => Shorthand (MoietyData a) where
--   shorthand (MoietyData x y) =
--     shorthand x <> shorthand y

renderChain :: Show a => a -> [b] -> (b -> String) -> String
renderChain x dbs f =
  show x <> ":" <> show (length dbs) <> wrapParen dbInfo
    where
      dbsAsStr = f <$> dbs
      dbInfo = intercalate "," $ f <$> dbs

-- |CarbonChain represents a carbon chain.
data CarbonChain a = CarbonChain
  { _carbons        :: NumCarbons
  --, _moietyDataList :: [MoietyData a]
  , _doubleBonds    :: [DoubleBond a]
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeClassy ''CarbonChain

instance HasNumCarbons (CarbonChain a) where
  numCarbons = carbons

instance Shorthand a => Shorthand (CarbonChain a) where
  shorthand (CarbonChain x dbs) =
    renderChain x dbs shorthand

instance NNomenclature a => NNomenclature (CarbonChain a) where
  nNomenclature (CarbonChain x dbs) =
    renderChain x dbs nNomenclature

instance IsSaturated (CarbonChain a) where
  isSaturated (CarbonChain _ dbs) = Just $ null dbs

instance IsMonounsaturated (CarbonChain a) where
  isMonounsaturated (CarbonChain _ dbs) = Just $ length dbs == 1

instance IsPolyunsaturated (CarbonChain a) where
  isPolyunsaturated (CarbonChain _ dbs) = Just $ length dbs > 1

instance IsBisAllylic [DoubleBond a] => IsBisAllylic (CarbonChain a) where
  isBisAllylic cc =
    cc^.doubleBonds.to isBisAllylic

renderOmegaPositions :: [DoubleBond OmegaPosition] -> String
renderOmegaPositions dbs =
  if fromMaybe False (isBisAllylic dbs)
    then nNomenclature (maximum dbs)
    else wrapParen $ intercalate "," (nNomenclature <$> dbs)

instance ToElementalComposition (CarbonChain a) where
  toElementalComposition (CarbonChain n dbs) =
    mkElementalComposition
      [ (C, fromIntegral n)
      , (H, 2 * fromIntegral n + 1 - 2 * length dbs)
      ]
  charge _ = Just 0

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
    renderChain x (concat y) shorthand

instance NNomenclature a => NNomenclature (TwoCombinedChains a) where
  nNomenclature (TwoCombinedChains x y) =
    renderChain x (concat y) nNomenclature

instance ToElementalComposition (TwoCombinedChains a) where
  toElementalComposition (TwoCombinedChains n dbs) =
    mkElementalComposition
      [ (C, fromIntegral n)
      , (H, 2 * fromIntegral n + 2 - 2 * (length . concat) dbs)
      ]
  charge _ = Just 0

data ThreeCombinedChains a = ThreeCombinedChains
   { _threeCombinedNumCarbons     :: NumCarbons
   , _threeCombinedNumDoubleBonds :: [[DoubleBond a]] }
   deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeClassy ''ThreeCombinedChains

instance HasNumCarbons (ThreeCombinedChains a) where
  numCarbons = threeCombinedNumCarbons

instance Shorthand a => Shorthand (ThreeCombinedChains a ) where
  shorthand (ThreeCombinedChains x y) =
    renderChain x (concat y) shorthand

instance NNomenclature a => NNomenclature (ThreeCombinedChains a) where
  nNomenclature (ThreeCombinedChains x y) =
    renderChain x (concat y) nNomenclature

instance ToElementalComposition (ThreeCombinedChains a) where
  toElementalComposition (ThreeCombinedChains n dbs) =
    mkElementalComposition
      [ (C, fromIntegral n)
      , (H, 2 * fromIntegral n + 3 - 2 * (length . concat) dbs)
      ]
  charge _ = Just 0

data FourCombinedChains a = FourCombinedChains
   { _fourCombinedNumCarbons     :: NumCarbons
   , _fourCombinedNumDoubleBonds :: [[DoubleBond a]] }
   deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeClassy ''FourCombinedChains

instance HasNumCarbons (FourCombinedChains a) where
  numCarbons = fourCombinedNumCarbons

instance Shorthand a => Shorthand (FourCombinedChains a ) where
  shorthand (FourCombinedChains x y) =
    renderChain x (concat y) shorthand

instance NNomenclature a => NNomenclature (FourCombinedChains a) where
  nNomenclature (FourCombinedChains x y) =
    renderChain x (concat y) nNomenclature

instance ToElementalComposition (FourCombinedChains a) where
  toElementalComposition (FourCombinedChains n dbs) =
    mkElementalComposition
      [ (C, fromIntegral n)
      , (H, 2 * fromIntegral n + 4 - 2 * (length . concat) dbs)
      ]
  charge _ = Just 0

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

instance ToElementalComposition Linkage where
  toElementalComposition =
    \case
      Acyl    -> mkElementalComposition [(O, 2), (H, -2)]
      Alkyl   -> mkElementalComposition [(O, 1)]
      Alkenyl -> mkElementalComposition [(O, 1), (H, -2)]
  charge _ = Just 0

data SnPosition -- delete?
  = Sn1
  | Sn2
  | Sn3
  deriving (Show, Read, Eq, Ord, Enum)

makePrisms ''SnPosition

data Radyl a = Radyl
   { _radylLinkage     :: Linkage
   , _radylCarbonChain :: CarbonChain a }
   deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeClassy ''Radyl

instance Shorthand a => Shorthand (Radyl a) where
  shorthand (Radyl x y) =
    shorthand x <> shorthand y

instance NNomenclature a => NNomenclature (Radyl a) where
  nNomenclature (Radyl x y) =
    shorthand x <> nNomenclature y

instance IsBisAllylic [DoubleBond a] => IsBisAllylic (Radyl a) where
  isBisAllylic r =
    r^.radylCarbonChain.doubleBonds.to isBisAllylic

instance ToElementalComposition (Radyl a) where
  toElementalComposition (Radyl l cc) =
    toElementalComposition l <> toElementalComposition cc
  charge _ = Just 0

frequency :: Ord a => [a] -> [(Int, a)]
frequency list = (length &&& head) <$> group (sort list)

linkageStrList :: [Linkage] -> [(Int, Linkage)]
linkageStrList = frequency . filter (/= Acyl)

freq2shorthand :: (Eq a, Num a) => (a, Linkage) -> String
freq2shorthand (x, y)
   | x == 1 = shorthand y
   | x == 2 = 'd' : shorthand y
   | x == 3 = 't' : shorthand y
   | otherwise = error "Shouldn't happen"

links :: [Linkage] -> String
links x =
  intercalate "," $ freq2shorthand <$> linkageStrList x

data TwoCombinedRadyls a = TwoCombinedRadyls
   { _radylLinkages           :: [Linkage]
   , _twoCombinedCarbonChains :: TwoCombinedChains a
   } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeClassy ''TwoCombinedRadyls

instance Shorthand a => Shorthand (TwoCombinedRadyls a) where
  shorthand (TwoCombinedRadyls x y) =
    links x <> shorthand y

instance NNomenclature a => NNomenclature (TwoCombinedRadyls a) where
  nNomenclature (TwoCombinedRadyls x y) =
    links x <> nNomenclature y

instance ToElementalComposition (TwoCombinedRadyls a) where
  toElementalComposition (TwoCombinedRadyls l cc) =
    foldMap toElementalComposition l <> toElementalComposition cc
  charge _ = Just 0

data ThreeCombinedRadyls a = ThreeCombinedRadyls
   { _threeRadylLinkages        :: [Linkage]
   , _threeCombinedCarbonChains :: ThreeCombinedChains a
   } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeClassy ''ThreeCombinedRadyls

instance Shorthand a => Shorthand (ThreeCombinedRadyls a) where
  shorthand (ThreeCombinedRadyls x y) =
    links x <> shorthand y

instance NNomenclature a => NNomenclature (ThreeCombinedRadyls a) where
  nNomenclature (ThreeCombinedRadyls x y) =
    links x <> nNomenclature y

instance ToElementalComposition (ThreeCombinedRadyls a) where
  toElementalComposition (ThreeCombinedRadyls l cc) =
    foldMap toElementalComposition l <> toElementalComposition cc
  charge _ = Just 0

data FourCombinedRadyls a = FourCombinedRadyls
   { _fourRadylLinkages        :: [Linkage]
   , _fourCombinedCarbonChains :: FourCombinedChains a
   } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeClassy ''FourCombinedRadyls

instance Shorthand a => Shorthand (FourCombinedRadyls a) where
  shorthand (FourCombinedRadyls x y) =
    links x <> shorthand y

instance NNomenclature a => NNomenclature (FourCombinedRadyls a) where
  nNomenclature (FourCombinedRadyls x y) =
    links x <> nNomenclature y

instance ToElementalComposition (FourCombinedRadyls a) where
  toElementalComposition (FourCombinedRadyls l cc) =
    foldMap toElementalComposition l <> toElementalComposition cc
  charge _ = Just 0

data PhosphatePosition -- delete?
  = P3'
  | P4'
  | P5'
  deriving (Show, Read, Eq, Ord, Enum)

makePrisms ''PhosphatePosition

instance Shorthand PhosphatePosition where -- delete?
   shorthand =
     \case
       P3' -> "3'"
       P4' -> "4'"
       P5' -> "5'"

numberOfDoubleBond :: CarbonChain a -> NumDoubleBonds
numberOfDoubleBond c =
  c^.doubleBonds.to (NumDoubleBonds . fromIntegral . length)

data Glycerol a b c = Glycerol
  { _sn1 :: a
  , _sn2 :: b
  , _sn3 :: c
  } deriving (Show, Read, Eq, Ord, Functor)

makeClassy ''Glycerol

instance ( ToElementalComposition a
         , ToElementalComposition b
         , ToElementalComposition c
         ) => ToElementalComposition (Glycerol a b c) where
  toElementalComposition (Glycerol a b c) =
    mkElementalComposition [(C, 3), (H, 5)]
    <> toElementalComposition a
    <> toElementalComposition b
    <> toElementalComposition b
  charge (Glycerol a b c) = charge a +++ charge b +++ charge c

Just x +++ Just y = Just $ x + y
_ +++ _               = Nothing

instance Bifunctor (Glycerol a) where
  bimap f g (Glycerol a b c) = Glycerol a (f b) (g c)

newtype Phosphate a = Phosphate a -- delete?
  deriving (Show, Read, Eq, Ord)

instance ToElementalComposition a => ToElementalComposition (Phosphate a) where
  toElementalComposition (Phosphate a) =
     mkElementalComposition [(P, 1), (O, 4), (H, 1)]
     <> toElementalComposition a
  charge (Phosphate a) = charge a

data Hydrogen = Hydrogen -- delete?
  deriving (Show, Read, Eq, Ord)

instance ToElementalComposition Hydrogen where -- delete?
  toElementalComposition _ = mkElementalComposition [(H, 1)]
  charge _ = Just 0

data PhosphatidicAcid = PhosphatidicAcid
  deriving (Show, Read, Eq, Ord)

instance Shorthand PhosphatidicAcid where
  shorthand _ = "PA"

instance ToElementalComposition PhosphatidicAcid where
  toElementalComposition _ = mkElementalComposition [(H, 1)]
  charge _ = Just 0

data Phosphatidylethanolamine = Phosphatidylethanolamine
  deriving (Show, Read, Eq, Ord)

instance Shorthand Phosphatidylethanolamine where
  shorthand _ = "PE"

instance ToElementalComposition Phosphatidylethanolamine where
  toElementalComposition _ = mkElementalComposition [(H, 1)]
  charge _ = Just 0

data Phosphatidylcholine = Phosphatidylcholine
  deriving (Show, Read, Eq, Ord)

instance Shorthand Phosphatidylcholine where
  shorthand _ = "PC"

instance ToElementalComposition Phosphatidylcholine where
  toElementalComposition _ = mkElementalComposition [(H, 1)]
  charge _ = Just 0

data Phosphatidylglycerol = Phosphatidylglycerol
  deriving (Show, Read, Eq, Ord)

instance Shorthand Phosphatidylglycerol where
  shorthand _ = "PG"

instance ToElementalComposition Phosphatidylglycerol where
  toElementalComposition _ = mkElementalComposition [(H, 1)]
  charge _ = Just 0

data Phosphatidylgylcerolphosphate = Phosphatidylgylcerolphosphate
  deriving (Show, Read, Eq, Ord)

instance Shorthand Phosphatidylgylcerolphosphate where
  shorthand _ = "PGP"

instance ToElementalComposition Phosphatidylgylcerolphosphate where
  toElementalComposition _ = mkElementalComposition [(H, 1)]
  charge _ = Just 0

data Phosphatidylserine = Phosphatidylserine
  deriving (Show, Read, Eq, Ord)

instance Shorthand Phosphatidylserine where
  shorthand _ = "PS"

instance ToElementalComposition Phosphatidylserine where
  toElementalComposition _ = mkElementalComposition [(H, 1)]
  charge _ = Just 0

data Phosphatidylinositol = Phosphatidylinositol
  deriving (Show, Read, Eq, Ord)

instance Shorthand Phosphatidylinositol where
  shorthand _ = "PI"

instance ToElementalComposition Phosphatidylinositol where
  toElementalComposition _ = mkElementalComposition [(H, 1)]
  charge _ = Just 0

data PhosphatidylinositolMonophosphate
  = PhosphatidylinositolMonophosphate
  | Phosphatidylinositol3Phosphate
  | Phosphatidylinositol4Phosphate
  | Phosphatidylinositol5Phosphate
  deriving (Show, Read, Eq, Ord, Enum)

instance Shorthand PhosphatidylinositolMonophosphate where
  shorthand PhosphatidylinositolMonophosphate = "PIP"
  shorthand Phosphatidylinositol3Phosphate    = "PIP[3′]"
  shorthand Phosphatidylinositol4Phosphate    = "PIP[4′]"
  shorthand Phosphatidylinositol5Phosphate    = "PIP[5′]"

instance ToElementalComposition PhosphatidylinositolMonophosphate where
  toElementalComposition _ = mkElementalComposition [(H, 1)]
  charge _ = Just 0

data PhosphatidylinositolBisphosphate
  = PhosphatidylinositolBisphosphate
  | Phosphatidylinositol34Bisphosphate
  | Phosphatidylinositol35Bisphosphate
  | Phosphatidylinositol45Bisphosphate
  deriving (Show, Read, Eq, Ord, Enum)

instance Shorthand PhosphatidylinositolBisphosphate where
  shorthand PhosphatidylinositolBisphosphate   = "PIP2"
  shorthand Phosphatidylinositol34Bisphosphate = "PIP2[3′,4′]"
  shorthand Phosphatidylinositol35Bisphosphate = "PIP2[3′,5′]"
  shorthand Phosphatidylinositol45Bisphosphate = "PIP2[4′,5′]"

instance ToElementalComposition PhosphatidylinositolBisphosphate where
  toElementalComposition _ = mkElementalComposition [(H, 1)]
  charge _ = Just 0

data PhosphatidylinositolTrisphosphate = PhosphatidylinositolTrisphosphate
  deriving (Show, Read, Eq, Ord)

instance Shorthand PhosphatidylinositolTrisphosphate where
  shorthand _ = "PI3"

instance ToElementalComposition PhosphatidylinositolTrisphosphate where
  toElementalComposition _ = mkElementalComposition [(H, 1)]
  charge _ = Just 0

data Cardiolipin a = Cardiolipin
  { sn1' :: Radyl a
  , sn2' :: Radyl a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

data GlycerolHydroxyl = GlycerolHydroxyl
  deriving (Show, Read, Eq, Ord)

instance Shorthand GlycerolHydroxyl where
  shorthand _ = "0:0"

instance NNomenclature GlycerolHydroxyl where
  nNomenclature = shorthand

instance ToElementalComposition GlycerolHydroxyl where
  toElementalComposition _ = mkElementalComposition [(O, 1), (H, 1)]
  charge _ = Just 0

instance (Shorthand a, Shorthand b)
  => Shorthand (Glycerol a (Radyl b) (Radyl b)) where
  shorthand (Glycerol h r1 r2) =
    shorthand h <> " " <> shorthand r1 <> "/" <> shorthand r2

instance (Shorthand a, Shorthand b) => Shorthand (Glycerol a (Radyl b) GlycerolHydroxyl) where
  shorthand (Glycerol h r _) =
    shorthand h <> " " <> shorthand r <> "/0:0"

instance (Shorthand a, Shorthand b) => Shorthand (Glycerol a GlycerolHydroxyl (Radyl b)) where
  shorthand (Glycerol h _ r) =
    shorthand h <> " 0:0/" <> shorthand r

instance (Shorthand a, NNomenclature b)
  => NNomenclature (Glycerol a (Radyl b) (Radyl b)) where
  nNomenclature (Glycerol h r1 r2) =
    shorthand h <> " " <> nNomenclature r1 <> "/" <> nNomenclature r2

instance (Shorthand a, NNomenclature b)
  => NNomenclature (Glycerol a (Radyl b) GlycerolHydroxyl) where
  nNomenclature (Glycerol h r _) =
    shorthand h <> " " <> nNomenclature r <> "/0:0"

instance (Shorthand a, NNomenclature b)
  => NNomenclature (Glycerol a GlycerolHydroxyl (Radyl b)) where
  nNomenclature (Glycerol h _ r) =
    shorthand h <> " 0:0/" <> nNomenclature r

newtype ClassLevel = ClassLevel
  { _getClassLevel :: Integer
  } deriving (Eq, Ord, Enum, Num)

makeClassy ''ClassLevel

instance Show ClassLevel where
  show (ClassLevel n) = show n

data TwoRadyls a = TwoRadyls
  { radyl1Di :: Radyl a
  , radyl2Di :: Radyl a
  } deriving (Show, Functor, Foldable, Traversable)

makeClassy ''TwoRadyls

instance (Eq a, Ord a) => Eq (TwoRadyls a) where
  TwoRadyls r1 r2 == TwoRadyls r1' r2' =
    sort [r1, r2] == sort [r1', r2']

instance Ord a => Ord (TwoRadyls a) where
  TwoRadyls r1 r2 `compare` TwoRadyls r1' r2' =
    sort [r1, r2] `compare` sort [r1', r2']

instance Shorthand a => Shorthand (TwoRadyls a) where
  shorthand (TwoRadyls r1 r2) = shorthand r1 <> "_" <> shorthand r2

instance NNomenclature a => NNomenclature (TwoRadyls a) where
  nNomenclature (TwoRadyls r1 r2) = nNomenclature r1 <> "_" <> nNomenclature r2

instance ToElementalComposition (TwoRadyls a) where
  toElementalComposition (TwoRadyls r1 r2) =
    toElementalComposition r1 <> toElementalComposition r2
  charge _ = Just 0

data ThreeRadyls a = ThreeRadyls
  { radyl1Tri :: Radyl a
  , radyl2Tri :: Radyl a
  , radyl3Tri :: Radyl a
  } deriving (Show, Functor, Foldable, Traversable)

makeClassy ''ThreeRadyls

instance (Eq a, Ord a) => Eq (ThreeRadyls a) where
  ThreeRadyls r1 r2 r3 == ThreeRadyls r1' r2' r3' =
    sort [r1, r2, r3] == sort [r1', r2', r3']

instance Ord a => Ord (ThreeRadyls a) where
  ThreeRadyls r1 r2 r3 `compare` ThreeRadyls r1' r2' r3' =
    sort [r1, r2, r3] `compare` sort [r1', r2', r3']

instance Shorthand a => Shorthand (ThreeRadyls a) where
  shorthand (ThreeRadyls r1 r2 r3) =
    shorthand r1 <> "_" <> shorthand r2 <> shorthand r3

instance NNomenclature a => NNomenclature (ThreeRadyls a) where
  nNomenclature (ThreeRadyls r1 r2 r3) =
    nNomenclature r1 <> "_" <> nNomenclature r2 <> nNomenclature r3

instance ToElementalComposition (ThreeRadyls a) where
  toElementalComposition (ThreeRadyls r1 r2 r3) =
    toElementalComposition r1
    <> toElementalComposition r2
    <> toElementalComposition r3
  charge _ = Just 0

class AllRadyls t where
  allRadyls :: Applicative f => (Radyl a -> f (Radyl b)) -> t a -> f (t b)

instance AllRadyls TwoRadyls where
  allRadyls f (TwoRadyls a b) = TwoRadyls <$> f a <*> f b

instance AllRadyls ThreeRadyls where
  allRadyls f (ThreeRadyls a b c) = ThreeRadyls <$> f a <*> f b <*> f c
