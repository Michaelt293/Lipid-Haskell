{-|
Module      : BuildingBlocks
Description :
Copyright   : Copyright 2015 IMBCR pty ltd. All rights reserved.
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : experimental
-}

module BuildingBlocks where

import Data.List
import ElementIsotopes


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

data Geometry = Z | E deriving (Show, Eq, Ord)

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
    } deriving (Eq, Ord) 

data Linkage = Acyl
             | Alkyl
             | Alkenyl
           deriving (Eq, Ord)

data Radyl = Radyl { linkage     :: Linkage 
                   , carbonChain :: CarbonChain 
                   } deriving (Eq, Ord)


instance Functor MoietyInfo where
    fmap f Unknown = Unknown
    fmap f (Known x) = Known (f x) 

instance Show CarbonChain where
    show (SimpleCarbonChain a [(b, c)]) = show a ++ ":" ++ show (length [(b, c)]) ++ info
        where info = intercalate "," [printMoietyInfo b ++ printMoietyInfo c]
 
instance Show Radyl where
    show (Radyl a b) = printLinkage a ++ show b 


showLinkage :: Linkage -> [Char]
showLinkage Acyl = ""
showLinkage Alkyl = "O-"
showLinkage Alkenyl = "P-"

showMoietyInfo :: Show a => MoietyInfo a -> [Char]
showMoietyInfo Unknown = ""
showMoietyInfo (Known a) = show a

doubleBondNumber :: CarbonChain -> NumberOfDoubleBonds
doubleBondNumber chain = fromIntegral $ length $ doubleBondData chain

doubleBondPositions :: CarbonChain -> [MoietyInfo OmegaPosition]
doubleBondPositions chain = map fst $ doubleBondData chain

doubleBondGeometries :: CarbonChain -> [MoietyInfo Geometry]
doubleBondGeometries chain = map snd $ doubleBondData chain










