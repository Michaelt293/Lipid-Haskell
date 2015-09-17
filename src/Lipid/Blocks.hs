{-|
Module      : Blocks
Description : Module contains data types used to construct lipid data types.
              In addition, the classes Shorthand and Nomenclature are defined.
Copyright   : Copyright 2015 IMBCR pty ltd. All rights reserved.
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : experimental
-}

module Lipid.Blocks
    (
      NumberOfCarbons(..)
    , NumberOfDoubleBonds(..)
    , Position(..)
    , Moiety(..)
    , DoubleBond(..)
    , Geometry(..)
    , CarbonChain(..)
    , CombinedChains(..)
    , Linkage(..)
    , Radyl(..)
    , CombinedRadyls(..)
    , SnPosition(..)
    , PhosphatePosition(..)
    , Shorthand(..)
    , Nomenclature(..)
    , doubleBondNumber
    , doubleBondPositions
    , doubleBondGeometries
    , toDelta
    , toOmega
    ) where

import qualified Data.List as List
import Lipid.Format

data NumberOfCarbons     = Carbons Integer
                         deriving (Show, Eq, Ord)

data NumberOfDoubleBonds = DoubleBonds Integer
                         deriving (Show, Eq, Ord)

data Position = Omega Integer
              | Delta Integer
              deriving (Show, Eq, Ord)

data Geometry = Cis | Trans deriving (Show, Eq, Ord)

data Moiety   = Hydroxyl
              | Keto
              | Methyl
              deriving (Show, Eq, Ord)

data DoubleBond = DoubleBond          { dbPosition          :: (Maybe Position) 
                                      , geometry            :: (Maybe Geometry) }
                                      deriving (Show, Eq, Ord)

data MoietyData = MoietyData          { moiety              :: Moiety 
                                      , moietyPosition      :: (Maybe Position) }
                                      deriving (Show, Eq, Ord)

data CarbonChain = SimpleCarbonChain  { carbonNumber        :: NumberOfCarbons
                                      , doubleBonds         :: [DoubleBond] }
                 |
                   ComplexCarbonChain  { carbonNumber       :: NumberOfCarbons
                                       , doubleBonds        :: [DoubleBond]
                                       , moietyData         :: [MoietyData] }
                                       deriving (Show, Eq, Ord)

data CombinedChains = CombinedChains  { combinedCNumber     :: NumberOfCarbons
                                      , combinedDoubleBonds :: [DoubleBond] }
                                      deriving (Show, Eq, Ord)
                                     -- This should be a list of lists

data Linkage = Acyl
             | Alkyl
             | Alkenyl
             deriving (Show, Eq, Ord)

data Radyl         = Radyl           { linkage     :: Linkage
                                     , carbonChain :: CarbonChain }
                                     deriving (Show, Eq, Ord)

data CombinedRadyls = CombinedRadyls { linkages       :: [Linkage]
                                     , combinedChains :: CombinedChains }
                                     deriving (Show, Eq, Ord)

                                     
data SnPosition = Sn1 | Sn2 | Sn3 deriving (Show, Eq, Ord)

data PhosphatePosition = P3' | P4' | P5' deriving (Show, Eq, Ord)


class Shorthand a where
    showShorthand :: a -> String

class Nomenclature a where
    showNnomenclature :: a -> String


instance Shorthand Integer where
    showShorthand = show

instance Shorthand NumberOfCarbons where
    showShorthand (Carbons x) = show x

instance Shorthand NumberOfDoubleBonds where
    showShorthand (DoubleBonds x) = show x

instance Shorthand Position where
    showShorthand (Omega x) = "n-" ++ show x
    showShorthand (Delta x) = show x

instance Shorthand Geometry where
    showShorthand Cis   = "Z"
    showShorthand Trans = "E"

instance Shorthand DoubleBond where
    showShorthand (DoubleBond Nothing _)                 = ""
    showShorthand (DoubleBond (Just (Omega x)) _)        = showShorthand (Omega x)
    showShorthand (DoubleBond (Just (Delta x)) Nothing)  = show x
    showShorthand (DoubleBond (Just (Delta x)) (Just y)) = show x ++ showShorthand y

instance Shorthand MoietyData where
    showShorthand (MoietyData x Nothing)  = showShorthand x
    showShorthand (MoietyData x (Just y)) = showShorthand y ++ showShorthand x

instance Shorthand Moiety where
    showShorthand Hydroxyl = "OH"
    showShorthand Keto = "O"
    showShorthand Methyl = "Me"

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
                  mInfo  = List.intercalate "," $ map showShorthand $ List.sort z

instance Nomenclature CarbonChain where
    showNnomenclature (SimpleCarbonChain x y)    =
        showShorthand x ++ ":" ++ show (length y) ++ wrapParen dbInfo
            where dbInfo = List.intercalate "," $ map showShorthand $ List.sort $ toOmega x y
    showNnomenclature (ComplexCarbonChain x y z) =
        showShorthand x ++ ":" ++ show (length y) ++ wrapParen dbInfo ++ wrapParen mInfo
            where dbInfo = List.intercalate "," $ map showShorthand $ List.sort $ toOmega x y
                  mInfo  = List.intercalate "," $ map showShorthand $ List.sort z

instance Shorthand CombinedChains where
    showShorthand (CombinedChains x y) =
        showShorthand x ++ ":" ++ show (length y) ++ wrapParen dbInfo
            where dbInfo = List.intercalate "," $ map showShorthand $ List.sort y 
            -- This needs to be changed y is a list of lists

instance Nomenclature CombinedChains where
    showNnomenclature (CombinedChains x y) =
        showShorthand x ++ ":" ++ show (length y) ++ wrapParen dbInfo
            where dbInfo = List.intercalate "," $ map showShorthand $ List.sort y 

instance Shorthand Radyl where
    showShorthand (Radyl x y)              = showShorthand x ++ showShorthand y

instance Nomenclature Radyl where
    showNnomenclature (Radyl x y)          = showShorthand x ++ showNnomenclature y

instance Shorthand CombinedRadyls where
    showShorthand (CombinedRadyls x y)     = links ++ showShorthand y
        where links =  List.intercalate "," $ map freq2shorthand $ linkageStrList x

instance Nomenclature CombinedRadyls where
    showNnomenclature (CombinedRadyls x y) = links ++ showNnomenclature y
        where links =  List.intercalate "," $ map freq2shorthand $ linkageStrList x

--From Stackoverflow
frequency :: Ord t => [t] -> [(Int, t)]
frequency list = map (\l -> (length l, head l)) (List.group (List.sort list))

linkageStrList :: Shorthand a => [a] -> [(Int, [Char])]
linkageStrList list = frequency $ filter (\x -> x /= "") $ map showShorthand list

freq2shorthand :: (Eq a, Num a) => (a, [Char]) -> [Char]
freq2shorthand (x, y)
    | x == 1 = y
    | x == 2 = 'd' : y
    | x == 3 = 't' : y

instance Shorthand PhosphatePosition where
    showShorthand P3' = "3'"
    showShorthand P4' = "4'"
    showShorthand P5' = "5'"


toDelta :: NumberOfCarbons -> [DoubleBond] -> [DoubleBond]
toDelta (Carbons n) positions = map (convert (Carbons n)) positions
    where convert (Carbons n) (DoubleBond Nothing y) = DoubleBond Nothing y
          convert (Carbons n) (DoubleBond (Just (Delta x)) y) = DoubleBond (Just (Delta x)) y
          convert (Carbons n) (DoubleBond (Just (Omega x)) y) = DoubleBond (Just (Delta (n - x))) y

toOmega :: NumberOfCarbons -> [DoubleBond] -> [DoubleBond]
toOmega (Carbons n) positions = map (convert (Carbons n)) positions
    where convert (Carbons n) (DoubleBond Nothing y) = DoubleBond Nothing y
          convert (Carbons n) (DoubleBond (Just (Delta x)) y) = DoubleBond (Just (Omega (n - x))) y
          convert (Carbons n) (DoubleBond (Just (Omega x)) y) = DoubleBond (Just (Omega x)) y

doubleBondNumber :: CarbonChain -> NumberOfDoubleBonds
doubleBondNumber chain = DoubleBonds $ fromIntegral . length . doubleBonds $ chain

doubleBondPositions :: CarbonChain -> [Maybe Position]
doubleBondPositions chain = map dbPosition $ doubleBonds chain

doubleBondGeometries :: CarbonChain -> [Maybe Geometry]
doubleBondGeometries chain = map geometry $ doubleBonds chain






