{-|
Module      : Formula.Blocks
Description : MolecularFormula data type and operators for working with molecular
              formulae are defined. Building blocks for constructing whole lipids
              provided.
Copyright   : Michael Thomas
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental
-}

module Formula.Blocks where

import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Control.Applicative
import ElementIsotopes hiding (monoisotopicMass, nominalMass)
import qualified ElementIsotopes as Elem (monoisotopicMass, nominalMass)
import Lipid.Blocks


data MolecularFormula = MolecularFormula (Map.Map ElementSymbol Integer)
                      deriving (Show, Eq, Ord)

class MolecularFormulae a where
    getFormula :: a -> Maybe MolecularFormula


(|+|) :: MolecularFormula -> MolecularFormula -> MolecularFormula
(|+|) (MolecularFormula m1) (MolecularFormula m2) =
           MolecularFormula $ Map.unionsWith (+) [m1, m2]

(|*|) :: Integer -> MolecularFormula -> MolecularFormula
(|*|) x (MolecularFormula m) = MolecularFormula $
           Map.fromList [(e, (find e m) * x) | e <- Map.keys m ]

(|-|) :: MolecularFormula -> MolecularFormula -> MolecularFormula
(|-|) (MolecularFormula m1) (MolecularFormula m2) =
           MolecularFormula $ Map.unionsWith (-) [m1, m2]

infixl 6 |+|

infixl 7 |*|

infixl 6 |-|

data BlockComp = BlockProton
               | BlockHydroxyl
               | BlockGlycerolBackbone
               | BlockPhosphate
               | BlockCholine
               | BlockSerine
               | BlockGlycerol
               | BlockGlycerolPhosphate
               | BlockEthanolamine
               | BlockInositol
               | BlockInositolPhosphate
               | BlockInositolDiPhosphate
               | BlockInositolTriPhosphate
               deriving (Eq, Ord, Show)

blockComposition = Map.fromList
    [ (BlockProton,               [ (H, 1)
                                  ])
    , (BlockHydroxyl,             [ (H, 1)
                                  , (O, 1)
                                  ])
    , (BlockGlycerolBackbone,     [ (C, 3)
                                  , (H, 5)
                                  ])
    , (BlockPhosphate,            [ (H, 1)
                                  , (P, 1)
                                  , (O, 4)
                                  ])
    , (BlockCholine,              [ (C, 5)
                                  , (N, 1)
                                  , (H, 13)
                                  ])
    , (BlockSerine,               [ (C, 3)
                                  , (N, 1)
                                  , (O, 2)
                                  , (H, 6)
                                  ])
    , (BlockGlycerol,             [ (C, 3)
                                  , (O, 2)
                                  , (H, 7)
                                  ])
    , (BlockGlycerolPhosphate,    [ (C, 3)
                                  , (P, 1)
                                  , (O, 5)
                                  , (H, 2)
                                  ])
    , (BlockEthanolamine,         [ (C, 2)
                                  , (N, 1)
                                  , (H, 6)
                                  ])
    , (BlockInositol,             [ (C, 6)
                                  , (O, 5)
                                  , (H, 11)
                                  ])
    , (BlockInositolPhosphate,    [ (C, 6)
                                  , (P, 1)
                                  , (O, 8)
                                  , (H, 13)
                                  ])
    , (BlockInositolDiPhosphate,  [ (C, 6)
                                  , (P, 2)
                                  , (O, 11)
                                  , (H, 15)
                                  ])
    , (BlockInositolTriPhosphate, [ (C, 6)
                                  , (P, 3)
                                  , (O, 14)
                                  , (H, 17)
                                  ])
    ]


instance MolecularFormulae Moiety where
    getFormula m = Just $ MolecularFormula $ Map.fromList $
        case m of
            Hydroxyl -> [(O, 1)]
            Keto     -> [(O, 1), (H, -2)]
            Methyl   -> [(C, 1), (H, 2)]
            Criegee  -> [(O, 2), (H, -2)]

instance MolecularFormulae Linkage where
    getFormula l = Just $ MolecularFormula $ Map.fromList $
        case l of
            Acyl    -> [(O, 2), (H, -2)]
            Alkyl   -> [(O, 1)]
            Alkenyl -> [(O, 1), (H, -2)]

instance MolecularFormulae CarbonChain where
    getFormula (SimpleCarbonChain (Carbons c) db)
        = Just $ MolecularFormula $ Map.fromList [ (C, c)
        , (H, c * 2 + 1 - 2 * fromIntegral (length db)) ]
    getFormula (ComplexCarbonChain (Carbons c) db ms)
        = sumFormula ([ Just (MolecularFormula $ Map.fromList [ (C, c)
                     , (H, c * 2 + 1 - 2 * fromIntegral (length db)) ])
                     ] ++ map (getFormula . moiety) ms)

instance MolecularFormulae CombinedChains where
    getFormula (CombinedChains (Carbons c) db) =
        Just $ MolecularFormula $ Map.fromList [ (C, c)
        , (H, c * 2 + 2 - 2 * fromIntegral (length db)) ]

instance MolecularFormulae Radyl where
    getFormula (Radyl l r) =  sumFormula [getFormula l,
                                          getFormula r]

instance MolecularFormulae CombinedRadyls where
    getFormula (CombinedRadyls ls r) = sumFormula $ getFormula r : map getFormula ls


find k v = Maybe.fromJust $ Map.lookup k v

lookupBlockComp :: BlockComp -> Maybe MolecularFormula 
lookupBlockComp n = do
     lst <- Map.lookup n blockComposition
     return $ MolecularFormula $ Map.fromList lst

sumFormula :: [Maybe MolecularFormula] -> Maybe MolecularFormula
sumFormula = foldl1 (\acc x -> liftA2 (|+|) acc x)




