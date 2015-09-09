{-|
Module      : BuildingBlocks
Description :
Copyright   : Copyright 2015 IMBCR pty ltd. All rights reserved.
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : experimental
-}

module BuildingBlocks
    (
      NumberOfCarbons
    , NumberOfDoubleBonds
    , Moiety(..)
    , Geometry(..)
    , CarbonChain(..)
    , CombinedChains
    , Linkage(..)
    , Radyl
    , CombinedRadyls
    , SnPosition
    , Shorthand(..)
    , Nomenclature(..)
    , wrap
    , wrapParen
    , doubleBondNumber
    , doubleBondPositions
    , doubleBondGeometries
    , toDelta
    , toOmega
    ) where

import qualified Data.List as List


type NumberOfCarbons = Integer
type NumberOfDoubleBonds = Integer

data Position = Omega Integer
              | Delta Integer
              deriving (Show, Eq, Ord)

data Geometry = Cis | Trans deriving (Show, Eq, Ord)

data Moiety = OH
            | O
            | Me
            deriving (Show, Eq, Ord)

data DoubleBond = DoubleBond { dbPosition            :: (Maybe Position) 
                             , geometry              :: (Maybe Geometry) }
                             deriving (Show, Eq, Ord)

data MoietyData = MoietyData { moiety                :: Moiety 
                             , moietyPosition        :: (Maybe Position) }
                             deriving (Show, Eq, Ord)

data CarbonChain = SimpleCarbonChain { carbonNumber  :: NumberOfCarbons
                                     , doubleBonds   :: [DoubleBond] }
                 |
                   ComplexCarbonChain { carbonNumber :: NumberOfCarbons
                                      , doubleBonds  :: [DoubleBond]
                                      , moietyData   :: [MoietyData] }
                                      deriving (Show, Eq, Ord)

data CombinedChains = CombinedChains { combinedCNumber     :: NumberOfCarbons
                                     , combinedDoubleBonds :: [DoubleBond] }
                                     deriving (Show, Eq, Ord)

                                     -- This should be a list of lists

data Linkage = Acyl
             | Alkyl
             | Alkenyl
             deriving (Show, Eq, Ord)

data Radyl = Radyl { linkage     :: Linkage
                   , carbonChain :: CarbonChain }
                   deriving (Show, Eq, Ord)

data CombinedRadyls = CombinedRadyls { linkages       :: [Linkage]
                                     , combinedChains :: CombinedChains }
                                     deriving (Show, Eq, Ord)

                                     
data SnPosition = Sn1 | Sn2 | Sn3 deriving (Show, Eq, Ord)


class Shorthand a where
    showShorthand :: a -> String

class Nomenclature a where
    showNnomenclature :: a -> String


instance Shorthand Integer where
    showShorthand = show

instance Shorthand Position where
    showShorthand (Omega x) = "n-" ++ show x
    showShorthand (Delta x) = show x

instance Shorthand Geometry where
    showShorthand Cis   = "Z"
    showShorthand Trans = "E"

instance Shorthand DoubleBond where
    showShorthand (DoubleBond Nothing _) = ""
    showShorthand (DoubleBond (Just (Omega x)) _) = showShorthand (Omega x)
    showShorthand (DoubleBond (Just (Delta x)) Nothing) = show x
    showShorthand (DoubleBond (Just (Delta x)) (Just y)) = show x ++ showShorthand y

instance Shorthand MoietyData where
    showShorthand (MoietyData x Nothing)  = showShorthand x
    showShorthand (MoietyData x (Just y)) = showShorthand y ++ showShorthand x

instance Shorthand Moiety where
    showShorthand = show

instance Shorthand Linkage where
    showShorthand Acyl    = ""
    showShorthand Alkyl   = "O-"
    showShorthand Alkenyl = "P-"

instance Shorthand CarbonChain where
    showShorthand (SimpleCarbonChain x y)    =
        showShorthand x ++ ":" ++ show (length y) ++ wrapParen dbInfo
            where dbInfo = List.intercalate "," $ map showShorthand $ List.sort $ toDelta x y
    showShorthand (ComplexCarbonChain x y z) =
        showShorthand x ++ ":" ++ show (length y) ++ wrapParen dbInfo ++ wrapParen mInfo
            where dbInfo = List.intercalate "," $ map showShorthand $ List.sort $ toDelta x y
                  mInfo = List.intercalate "," $ map showShorthand $ List.sort z

instance Nomenclature CarbonChain where
    showNnomenclature (SimpleCarbonChain x y)    =
        showShorthand x ++ ":" ++ show (length y) ++ wrapParen dbInfo
            where dbInfo = List.intercalate "," $ map showShorthand $ List.sort $ toOmega x y
    showNnomenclature (ComplexCarbonChain x y z) =
        showShorthand x ++ ":" ++ show (length y) ++ wrapParen dbInfo ++ wrapParen mInfo
            where dbInfo = List.intercalate "," $ map showShorthand $ List.sort $ toOmega x y
                  mInfo = List.intercalate "," $ map showShorthand $ List.sort z

instance Shorthand CombinedChains where
    showShorthand (CombinedChains x y) =
        showShorthand x ++ ":" ++ show (length y) ++ wrapParen dbInfo
            where dbInfo = List.intercalate "," $ map showShorthand $ List.sort y 
            -- This needs to be changed y is a list of lists

instance Shorthand Radyl where
    showShorthand (Radyl x y) = showShorthand x ++ showShorthand y

instance Nomenclature Radyl where
    showNnomenclature (Radyl x y) = showShorthand x ++ showNnomenclature y


wrap :: [Char] -> [Char] -> [Char] -> [Char]
wrap open close str = if length str == 0 
                    then ""
                    else open ++ str ++ close

wrapParen :: [Char] -> [Char]
wrapParen str = wrap "(" ")" str

toDelta :: NumberOfCarbons -> [DoubleBond] -> [DoubleBond]
toDelta num positions = map (convert num) positions
    where convert num (DoubleBond Nothing y) = DoubleBond Nothing y
          convert num (DoubleBond (Just (Delta x)) y) = DoubleBond (Just (Delta x)) y
          convert num (DoubleBond (Just (Omega x)) y) = DoubleBond (Just (Delta (num - x))) y

toOmega :: NumberOfCarbons -> [DoubleBond] -> [DoubleBond]
toOmega num positions = map (convert num) positions
    where convert num (DoubleBond Nothing y) = DoubleBond Nothing y
          convert num (DoubleBond (Just (Delta x)) y) = DoubleBond (Just (Omega (num - x))) y
          convert num (DoubleBond (Just (Omega x)) y) = DoubleBond (Just (Omega x)) y

doubleBondNumber :: CarbonChain -> NumberOfDoubleBonds
doubleBondNumber chain = fromIntegral . length . doubleBonds $ chain

doubleBondPositions :: CarbonChain -> [Maybe Position]
doubleBondPositions chain = map dbPosition $ doubleBonds chain

doubleBondGeometries :: CarbonChain -> [Maybe Geometry]
doubleBondGeometries chain = map geometry $ doubleBonds chain







