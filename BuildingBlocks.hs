{-|
Module      : BuildingBlocks
Description :
Copyright   : Copyright 2015 IMBCR pty ltd. All rights reserved.
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : experimental
-}

--{-# LANGUAGE ExistentialQuantification #-}

module BuildingBlocks
    (
      NumberOfCarbons
    , NumberOfDoubleBonds
    , Moiety(..)
    , Geometry(..)
    , CarbonChain(..)
    , Linkage(..)
    , Radyl
    , Shorthand(..)
    , doubleBondNumber
    , doubleBondPositions
    , doubleBondGeometries
    , omegaToDelta
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

data DoubleBond = DoubleBond { dpPosition            :: (Maybe Position) 
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

data Linkage = Acyl
             | Alkyl
             | Alkenyl
             deriving (Show, Eq, Ord)

data Radyl = Radyl { linkage     :: Linkage
                   , carbonChain :: CarbonChain } 
                   deriving (Show, Eq, Ord)


class Shorthand a where
    showShorthand :: a -> String


instance Shorthand Integer where
    showShorthand = show

instance Shorthand Position where
    showShorthand (Omega x) = "n-" ++ show x
    showShorthand (Delta x) = show x

instance Shorthand Geometry where
    showShorthand Cis = "Z"
    showShorthand Trans = "E"

instance Shorthand DoubleBond where
    showShorthand (DoubleBond Nothing _) = ""
    showShorthand (DoubleBond (Just (Omega x)) _) = showShorthand (Omega x)
    showShorthand (DoubleBond (Just (Delta x)) Nothing) = show x
    showShorthand (DoubleBond (Just (Delta x)) (Just y)) = show x ++ showShorthand y

instance Shorthand MoietyData where
    showShorthand (MoietyData x Nothing) = showShorthand x
    showShorthand (MoietyData x (Just y)) = showShorthand x ++ showShorthand y
 

instance Shorthand Moiety where
    showShorthand = show

instance Shorthand Linkage where
    showShorthand Acyl = ""
    showShorthand Alkyl = "O-"
    showShorthand Alkenyl = "P-"

instance Shorthand CarbonChain where
    showShorthand (SimpleCarbonChain x y) =
        showShorthand x ++ ":" ++ show (length y) ++ wrapParen dbInfo 
            where dbInfo = List.intercalate "," $ map showShorthand y
    showShorthand (ComplexCarbonChain x y z) =
        showShorthand x ++ ":" ++ show (length y) ++ wrapParen dbInfo ++ wrapParen mInfo
            where dbInfo = List.intercalate "," $ map showShorthand y
                  mInfo = List.intercalate "," $ map showShorthand z

instance Shorthand Radyl where
    showShorthand (Radyl x y) = showShorthand x ++ showShorthand y


wrapParen :: [Char] -> [Char]
wrapParen str = if length str == 0 
                    then ""
                    else "(" ++ str ++ ")" 

doubleBondNumber :: CarbonChain -> NumberOfDoubleBonds
doubleBondNumber chain = fromIntegral . length . doubleBonds $ chain

doubleBondPositions :: CarbonChain -> [Maybe Position]
doubleBondPositions chain = map dpPosition $ doubleBonds chain

doubleBondGeometries :: CarbonChain -> [Maybe Geometry]
doubleBondGeometries chain = map geometry $ doubleBonds chain

--class DBNotation a where
 --   changeNotation :: CarbonChain -> MoietyInfo a


--instance (DBNotation a) => DBNotation (MoietyInfo a) where
  --  omegaToDelta :: CarbonChain -> [MoietyInfo DeltaPosition]
   -- omegaToDelta chain = fmap (\y -> fmap (\x -> carbonNumber chain - x) y) $ --doubleBondPositions chain











