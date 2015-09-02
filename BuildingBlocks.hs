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
    , DeltaPosition
    , OmegaPosition
    , MoietyInfo(..)
    , Moiety(..)
    , Geometry(..)
    , CarbonChain(..)
    , Linkage(..)
    , Radyl
    , Shorthand(..)
    , doubleBondNumber
    , doubleBondPositions
    , doubleBondGeometries
--    , omegaToDelta
    ) where

import qualified Data.List as List

type NumberOfCarbons = Integer
type NumberOfDoubleBonds = Integer
type DeltaPosition = Integer
type OmegaPosition = Integer


data MoietyInfo a = Unknown
                  | Known a
                deriving (Show, Eq, Ord)

data Moiety = OH
            | O
            | Me
          deriving (Show, Eq, Ord)

data Geometry = Cis | Trans deriving (Show, Eq, Ord)

data CarbonChain = 
    SimpleCarbonChain
    { carbonNumber   :: NumberOfCarbons
    , doubleBondData :: [(MoietyInfo OmegaPosition, MoietyInfo Geometry)]
    }
    |
    ComplexCarbonChain 
    { carbonNumber   :: NumberOfCarbons
    , doubleBondData :: [(MoietyInfo OmegaPosition, MoietyInfo Geometry)]
    , moietyData     :: [(Moiety, MoietyInfo DeltaPosition)]
    } deriving (Show, Eq, Ord)

data Linkage = Acyl
             | Alkyl
             | Alkenyl
           deriving (Show, Eq, Ord)

data Radyl = Radyl { linkage     :: Linkage
                   , carbonChain :: CarbonChain
                   } deriving (Show, Eq, Ord)


class Shorthand a where
    showShorthand :: a -> String


instance Functor MoietyInfo where
    fmap f Unknown = Unknown
    fmap f (Known x) = Known (f x)

instance Shorthand Integer where
    showShorthand = show

instance (Shorthand a) => Shorthand (MoietyInfo a) where
    showShorthand Unknown = ""
    showShorthand (Known x) = showShorthand x

instance Shorthand Moiety where
    showShorthand = show

instance Shorthand Geometry where
    showShorthand Cis = "Z"
    showShorthand Trans = "E"

instance Shorthand Linkage where
    showShorthand Acyl = ""
    showShorthand Alkyl = "O-"
    showShorthand Alkenyl = "P-"

instance Shorthand CarbonChain where
    showShorthand (SimpleCarbonChain x y) = 
        showShorthand x ++ ":" ++ show (length y) ++ wrap info
        where info = List.intercalate "," [showShorthand db ++ showShorthand g | (db, g) <- y]
              wrap d = if length d == 0 
                           then ""
                           else "(" ++ d ++ ")" 
    showShorthand (ComplexCarbonChain x y z) =  
        showShorthand x ++ ":" ++ show (length y) ++ wrap dbInfo ++ wrap mInfo
        where dbInfo = List.intercalate "," [showShorthand db ++ showShorthand g | (db, g) <- y]
              mInfo = List.intercalate "," [showShorthand m ++ showShorthand p | (m, p) <- z]
              wrap d = if length d == 0 
                           then ""
                           else "(" ++ d ++ ")" 

instance Shorthand Radyl where
    showShorthand (Radyl x y) = showShorthand x ++ showShorthand y


doubleBondNumber :: CarbonChain -> NumberOfDoubleBonds
doubleBondNumber chain = fromIntegral . length . doubleBondData $ chain

doubleBondPositions :: CarbonChain -> [MoietyInfo OmegaPosition]
doubleBondPositions chain = map fst $ doubleBondData chain

doubleBondGeometries :: CarbonChain -> [MoietyInfo Geometry]
doubleBondGeometries chain = map snd $ doubleBondData chain

--omegaToDelta chain = fmap (\x -> carbonNumber chain - x) doubleBondPositions chain










