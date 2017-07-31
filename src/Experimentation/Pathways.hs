{-|
Module      : Pathways
Description :
Copyright   : Michael Thomas
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : experimental
-}

module Pathways where

import BuildingBlocks
import LipidClasses
import Chains
import Formulae

class Desaturase a where
    delta5Desaturase :: a -> a
    delta6Desaturase :: a -> a
    delta8Desaturase :: a -> a

class Elongation a where
    elongation :: a -> a

class BetaOxidation a where
    betaOxidation :: a -> a

instance Desaturase FA where
    delta5Desaturase (FA x) = FA $ addDoubleBond 5 Cis x

    delta6Desaturase (FA x) = FA $ addDoubleBond 6 Cis x

    delta8Desaturase (FA x) = FA $ addDoubleBond 8 Cis x

instance Elongation FA where
    elongation (FA x) = FA $ chainElongate x

instance BetaOxidation FA where
    betaOxidation (FA x) = FA $ chainBetaOxidation x

addDoubleBond :: Integer -> Geometry -> CarbonChain -> CarbonChain
addDoubleBond position geometry (SimpleCarbonChain x y)
    | Just (Delta position) `elem` doubleBondPositions (SimpleCarbonChain x y) = SimpleCarbonChain x y
    | otherwise = SimpleCarbonChain x (DoubleBond (Just (Delta position) (Just geometry)) : y)

shiftPosition n (Delta p) = Delta (p + n)

shiftDoubleBond n (DoubleBond x y) = DoubleBond (fmap (shiftPosition n) x) y

chainElongate (SimpleCarbonChain x y) =
    SimpleCarbonChain (x + 2) (fmap (shiftDoubleBond 2) y)

chainBetaOxidation (SimpleCarbonChain x y) =
    SimpleCarbonChain (x - 2) (fmap (shiftDoubleBond (- 2)) y)
