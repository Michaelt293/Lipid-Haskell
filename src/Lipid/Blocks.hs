{-|
Module      : Lipid.Blocks
Description : Module contains lipid data types. In addition,
              the classes Shorthand and Nomenclature are defined.
Copyright   : Michael Thomas
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental
-}


module Lipid.Blocks
    (
      Carbons(..)
    , NumDoubleBonds(..)
    , Position(..)
    , Moiety(..)
    , DoubleBond(..)
    , MoietyData(..)
    , Geometry(..)
    , CarbonChain(..)
    , TwoCombinedChains(..)
    , ThreeCombinedChains(..)
    , FourCombinedChains(..)
    , Linkage(..)
    , Radyl(..)
    , TwoCombinedRadyls(..)
    , ThreeCombinedRadyls(..)
    , FourCombinedRadyls(..)
    , SnPosition(..)
    , PhosphatePosition(..)
    , Shorthand(..)
    , Nomenclature(..)
    , doubleBondNumber
    , doubleBondPositions
    , doubleBondGeometries
    , toDelta
    , isBisAllylic
    , dbToDelta
    , toOmega
    , dbToOmega
    , listToString
    , numberOfCarbonsAdd
    ) where

import           Control.Applicative
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import           Lipid.Format

-- |Carbons represents the number of carbons in a single carbon chain or
-- two or more combined chains.
newtype Carbons = Carbons Integer
                deriving (Show, Eq, Ord)

-- |NumDoubleBonds represents the number of carbon-carbon double bonds
-- on a carbon chain.
newtype NumDoubleBonds = NumDoubleBonds {numDoubleBonds :: Integer}
                       deriving (Show, Eq, Ord)

-- |Position represents the position of carbon-carbon double bonds or
-- moieties on a carbon chain. Positions can be provided from the
-- methyl end (Omega) or from the linkage (Delta).
data Position = Omega Integer
              | Delta Integer
              deriving (Show, Eq)

-- |Geometry represent the geometry of carbon-carbon double bonds.
data Geometry = Cis | Trans
              deriving (Show, Eq, Ord)

-- |Moiety represents moieties commonly found on carbon chains
-- (Hydroxyl, Keto and Methyl) as well as ozonolysis products
-- (Criegee and Ozonide) observed in OzID experiments.
data Moiety   = Hydroxyl
              | Keto
              | Methyl
              | Criegee
              | Ozonide
              deriving (Show, Eq, Ord)

-- |DoubleBond represents a carbon-carbon double bond as found on
-- carbon chains. A DoubleBond data type has two fields, Maybe Position
-- and Maybe Geometry. These fields use the Maybe type to
-- account for uncertainity (i.e., Nothing represents an unknown
-- position or geometry).
data DoubleBond = DoubleBond
    { dbPosition :: (Maybe Position)
    , geometry   :: (Maybe Geometry) }
    deriving (Show, Eq, Ord)

-- |MoietyData represents a moiety on a carbon chain. A MoietyData
-- data type has two fields, Moiety and Maybe Position. The use of
-- the Maybe type accounts for uncertainity (i.e., Nothing
-- represents and unknown moiety position).
data MoietyData = MoietyData
    { moiety         :: Moiety
    , moietyPosition :: (Maybe Position) }
    deriving (Show, Eq, Ord)

-- |CarbonChain represents a carbon chain. A carbon chain can be
-- a SimpleCarbonChain (no additional moieties) or a ComplexCarbonChain
-- (addition moieties). SimpleCarbonChain has two fields, Carbons and
-- [Dou]
data CarbonChain = SimpleCarbonChain  Carbons [DoubleBond]
                 | ComplexCarbonChain Carbons [DoubleBond] [MoietyData]
                 deriving (Show, Eq, Ord)

-- |TwoCombinedChains represents two combined carbon chains. For example,
-- a diradyl phosphatidylcholine can be written as PC 32:1
data TwoCombinedChains = TwoCombinedChains
    { twoCombinedCarbons     :: Carbons
    , twoCombinedDoubleBonds :: [[DoubleBond]] }
    deriving (Show, Eq, Ord)

data ThreeCombinedChains = ThreeCombinedChains
    { threeCombinedCarbons     :: Carbons
    , threeCombinedDoubleBonds :: [[DoubleBond]] }
    deriving (Show, Eq, Ord)

data FourCombinedChains = FourCombinedChains
    { fourCombinedCarbons     :: Carbons
    , fourCombinedDoubleBonds :: [[DoubleBond]] }
    deriving (Show, Eq, Ord)

data Linkage = Acyl
             | Alkyl
             | Alkenyl
             deriving (Show, Eq, Ord)

data Radyl = Radyl
    { linkage     :: Linkage
    , carbonChain :: CarbonChain }
    deriving (Show, Eq, Ord)

data TwoCombinedRadyls = TwoCombinedRadyls
    { twoLinkages       :: [Linkage]
    , twoCombinedChains :: TwoCombinedChains }
    deriving (Show, Eq, Ord)

data ThreeCombinedRadyls = ThreeCombinedRadyls
    { threeLinkages     :: [Linkage]
    , threeCombinedChains :: ThreeCombinedChains }
    deriving (Show, Eq, Ord)

data FourCombinedRadyls = FourCombinedRadyls
    { fourLinkages       :: [Linkage]
    , fourCombinedChains :: FourCombinedChains }
    deriving (Show, Eq, Ord)

data SnPosition = Sn1 | Sn2 | Sn3
                deriving (Show, Eq, Ord)

data PhosphatePosition = P3' | P4' | P5'
                       deriving (Show, Eq, Ord)


class Shorthand a where
    showShorthand :: a -> String

class Nomenclature a where
    showNnomenclature :: a -> String

instance Enum Carbons where
    toEnum x = Carbons (fromIntegral x)
    fromEnum (Carbons x) = fromIntegral x

instance Enum NumDoubleBonds where
    toEnum x = NumDoubleBonds (fromIntegral x)
    fromEnum (NumDoubleBonds x) = fromIntegral x

instance Enum Position where
    toEnum x = Delta (fromIntegral x)
    fromEnum (Delta x) = fromIntegral x

instance Ord Position where
    compare (Delta x) (Delta y) = compare x y
    compare (Omega x) (Omega y) = compare y x

instance Num Position where
    (+) (Omega x) (Omega y) = Omega (x + y)
    (+) (Delta x) (Delta y) = Delta (x + y)
    (-) (Omega x) (Omega y) = Omega (x - y)
    (-) (Delta x) (Delta y) = Delta (x - y)
    (*) (Omega x) (Omega y) = Omega (x * y)
    (*) (Delta x) (Delta y) = Delta (x * y)
    abs (Omega x) = Omega (abs x)
    abs (Delta x) = Delta (abs x)
    signum (Omega x) = Omega (signum x)
    signum (Delta x) = Delta (signum x)
    fromInteger x = Delta x

instance Shorthand Carbons where
    showShorthand (Carbons x) = show x

instance Shorthand NumDoubleBonds where
    showShorthand (NumDoubleBonds x) = show x

instance Shorthand Position where
    showShorthand (Omega x) = "n-" ++ show x
    showShorthand (Delta x) = show x

instance Shorthand Geometry where
    showShorthand Cis   = "Z"
    showShorthand Trans = "E"

instance Shorthand DoubleBond where
    showShorthand x
        = case x of
            (DoubleBond Nothing _)                 -> ""
            (DoubleBond (Just (Omega x)) _)        -> showShorthand (Omega x)
            (DoubleBond (Just (Delta x)) Nothing)  -> show x
            (DoubleBond (Just (Delta x)) (Just y)) -> show x ++ showShorthand y

instance Shorthand MoietyData where
    showShorthand x
        = case x of
              (MoietyData x Nothing)  -> showShorthand x
              (MoietyData x (Just y)) -> showShorthand y ++ showShorthand x

instance Shorthand Moiety where
    showShorthand x =
      case x of
        Hydroxyl -> "OH"
        Keto     -> "O"
        Methyl   -> "Me"
        Criegee  -> "Criegee"

instance Shorthand Linkage where
    showShorthand x =
      case x of
        Acyl    -> ""
        Alkyl   -> "O-"
        Alkenyl -> "P-"

instance Shorthand CarbonChain where
    showShorthand c =
      case c of
        (SimpleCarbonChain c dbs)     -> renderSimpleChain c dbs toDelta
        (ComplexCarbonChain c dbs ms) -> renderComplexChain c dbs ms toDelta

instance Nomenclature CarbonChain where
    showNnomenclature c =
      case c of
        (SimpleCarbonChain c dbs)     -> renderSimpleChain c dbs renderOmegaPositions
        (ComplexCarbonChain c dbs ms) -> renderComplexChain c dbs ms renderOmegaPositions

formatIntercalate l = List.intercalate "," $ map showShorthand $ List.sort l

listToString l
    | Nothing `elem` (map dbPosition l) = ""
    | otherwise        = formatIntercalate l

renderSimpleChain c dbs f
      = showShorthand c ++ ":" ++ show (length dbs) ++ wrapParen dbInfo
            where dbInfo = listToString $ f c dbs

renderComplexChain c dbs ms f
      = showShorthand c ++ ":" ++ show (length dbs)
        ++ wrapParen dbInfo ++ wrapParen mInfo
           where dbInfo = listToString $ f c dbs
                 mInfo  = formatIntercalate ms


isBisAllylic :: Carbons -> [DoubleBond] -> Bool
isBisAllylic c dbs = doubleBondList == (take (length dbs) $ Just <$> [Delta 0, Delta 3 ..])
    where sortedDoubleBondList = List.sort $ dbPosition <$> List.sort (toDelta c dbs)
          minDoubleBond = minimum sortedDoubleBondList
          doubleBondList = fmap (liftA2 (\x y -> y - x) minDoubleBond) sortedDoubleBondList

renderOmegaPositions c dbs
   = case (isBisAllylic c dbs) of
          True -> toOmega c [maximum dbs]
          False -> toOmega c dbs

instance Shorthand TwoCombinedChains where
    showShorthand (TwoCombinedChains x y) = renderCombinedChains x y

instance Nomenclature TwoCombinedChains where
    showNnomenclature (TwoCombinedChains x y) = renderCombinedChains x y

instance Shorthand ThreeCombinedChains where
    showShorthand (ThreeCombinedChains x y) = renderCombinedChains x y

instance Shorthand FourCombinedChains where
    showShorthand (FourCombinedChains x y) = renderCombinedChains x y

instance Nomenclature ThreeCombinedChains where
    showNnomenclature (ThreeCombinedChains x y) = renderCombinedChains x y

instance Nomenclature FourCombinedChains where
    showNnomenclature (FourCombinedChains x y) = renderCombinedChains x y

renderCombinedChains x y = showShorthand x ++ ":" ++ show (length y) ++ wrapParen dbInfo
            where dbInfo = List.intercalate "," $ map showShorthand $ concat $ List.sort y

instance Shorthand Radyl where
    showShorthand (Radyl x y)              = showShorthand x ++ showShorthand y

instance Nomenclature Radyl where
    showNnomenclature (Radyl x y)          = showShorthand x ++ showNnomenclature y

instance Shorthand TwoCombinedRadyls where
    showShorthand (TwoCombinedRadyls x y)     = links x ++ showShorthand y

instance Nomenclature TwoCombinedRadyls where
    showNnomenclature (TwoCombinedRadyls x y) = links x ++ showNnomenclature y

instance Shorthand ThreeCombinedRadyls where
    showShorthand (ThreeCombinedRadyls x y)     = links x ++ showShorthand y

instance Nomenclature ThreeCombinedRadyls where
    showNnomenclature (ThreeCombinedRadyls x y) = links x ++ showNnomenclature y

instance Shorthand FourCombinedRadyls where
    showShorthand (FourCombinedRadyls x y)     = links x ++ showShorthand y

instance Nomenclature FourCombinedRadyls where
    showNnomenclature (FourCombinedRadyls x y) = links x ++ showNnomenclature y

-- Helper functions are defined below. frequency function from a Stackoverflow reply.
frequency :: Ord t => [t] -> [(Int, t)]
frequency list = map (\l -> (length l, head l)) (List.group (List.sort list))

linkageStrList :: Shorthand a => [a] -> [(Int, [Char])]
linkageStrList list = frequency $ filter (\x -> x /= "") $ map showShorthand list

freq2shorthand :: (Eq a, Num a) => (a, [Char]) -> [Char]
freq2shorthand (x, y)
    | x == 1 = y
    | x == 2 = 'd' : y
    | x == 3 = 't' : y

links x = List.intercalate "," $ map freq2shorthand $ linkageStrList x

instance Shorthand PhosphatePosition where
    showShorthand x = case x of
                          P3' -> "3'"
                          P4' -> "4'"
                          P5' -> "5'"


toDelta :: Carbons -> [DoubleBond] -> [DoubleBond]
toDelta cs positions = map (dbToDelta cs) positions

dbToDelta :: Carbons -> DoubleBond -> DoubleBond
dbToDelta _ db@(DoubleBond Nothing _)  = db
dbToDelta (Carbons n) (DoubleBond (Just (Omega x)) y)
                                       = DoubleBond (Just (Delta (n - x))) y
dbToDelta _ db = db

toOmega :: Carbons -> [DoubleBond] -> [DoubleBond]
toOmega (Carbons n) positions = map (dbToOmega (Carbons n)) positions

dbToOmega :: Carbons -> DoubleBond -> DoubleBond
dbToOmega _ db@(DoubleBond Nothing _)  = db
dbToOmega (Carbons n) (DoubleBond (Just (Delta x)) y)
                                       = DoubleBond (Just (Omega (n - x))) y
dbToOmega _ db = db

doubleBondNumber :: CarbonChain -> NumDoubleBonds
doubleBondNumber chain = NumDoubleBonds $ fromIntegral . length . doubleBonds $ chain

doublebonds (SimpleCarbonChain _ dbs) = dbs
doubleBonds (ComplexCarbonChain _ dbs _) = dbs

doubleBondPositions :: CarbonChain -> [Maybe Position]
doubleBondPositions chain = map dbPosition $ doubleBonds chain

doubleBondGeometries :: CarbonChain -> [Maybe Geometry]
doubleBondGeometries chain = map geometry $ doubleBonds chain

numberOfCarbonsAdd f (Carbons x) = Carbons (f x)







