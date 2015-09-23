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
import ElementIsotopes hiding (monoisotopicMass, nominalMass)
import qualified ElementIsotopes as Elem (monoisotopicMass, nominalMass)
import Lipid.Blocks


data MolecularFormula = MolecularFormula (Map.Map ElementSymbol Integer)
                      deriving (Show, Eq, Ord)

class MolecularFormulae a where
    getFormula :: a -> Maybe MolecularFormula


(|+|) :: Maybe MolecularFormula -> Maybe MolecularFormula -> Maybe MolecularFormula
(|+|) Nothing _ = Nothing
(|+|) _ Nothing = Nothing
(|+|) (Just (MolecularFormula m1)) (Just (MolecularFormula m2)) = 
           Just $ MolecularFormula $ Map.unionsWith (+) [m1, m2]

(|*|) :: Integer -> Maybe MolecularFormula -> Maybe MolecularFormula
(|*|) _ Nothing = Nothing
(|*|) x (Just (MolecularFormula m)) = Just $ MolecularFormula $ 
                                      Map.fromList [(e, (find e m) * x) | e <- Map.keys m ]

(|-|) :: Maybe MolecularFormula -> Maybe MolecularFormula -> Maybe MolecularFormula
(|-|) Nothing _ = Nothing
(|-|) _ Nothing = Nothing
(|-|) (Just (MolecularFormula m1)) (Just (MolecularFormula m2)) = 
           Just $ MolecularFormula $ Map.unionsWith (-) [m1, m2]

infixl 6 |+|

infixl 7 |*|

infixl 6 |-|

blockComposition = Map.fromList
    [ ("proton",          Just (MolecularFormula (Map.fromList [(H, 1)])))
    , ("hydroxyl",        Just (MolecularFormula (Map.fromList [(H, 1),
                                                                (O, 1)])))
    , ("glycerolBackbone", Just (MolecularFormula (Map.fromList [(C, 3),
                                                                (H, 5)])))
    , ("phosphate",       Just (MolecularFormula (Map.fromList [(H, 1),
                                                                (P, 1),
                                                                (O, 4)])))
    , ("choline",         Just (MolecularFormula (Map.fromList [(C, 5),
                                                                (N, 1),
                                                                (H, 13)])))
    , ("serine",          Just (MolecularFormula (Map.fromList [(C, 3),
                                                                (N, 1),
                                                                (O, 2),
                                                                (H, 6)])))
    , ("glycerol",        Just (MolecularFormula (Map.fromList [(C, 3),
                                                                (O, 2),
                                                                (H, 7)])))
    , ("glycerolPhosphate", Just (MolecularFormula (Map.fromList [(C, 3),
                                                                  (P, 1),
                                                                  (O, 5),
                                                                  (H, 2)])))
    , ("ethanolamine",    Just (MolecularFormula (Map.fromList [(C, 2),
                                                                (N, 1),
                                                                (H, 6)])))
    , ("inositol",        Just (MolecularFormula (Map.fromList [(C, 6),
                                                                (O, 5),
                                                                (H, 11)])))
    , ("inositolPhosphate", Just (MolecularFormula (Map.fromList [(C, 6),
                                                                  (P, 1),
                                                                  (O, 8),
                                                                  (H, 13)])))
    , ("inositolDiPhosphate", Just (MolecularFormula (Map.fromList [(C, 6),
                                                                    (P, 2),
                                                                    (O, 11),
                                                                    (H, 15)])))
    , ("inositolTriPhosphate", Just (MolecularFormula (Map.fromList [(C, 6),
                                                                  (P, 3),
                                                                  (O, 14),
                                                                  (H, 17)])))
    ]


instance MolecularFormulae Moiety where
    getFormula Hydroxyl = Just $ MolecularFormula $ Map.fromList [(O, 1)]
    getFormula Keto     = Just $ MolecularFormula $ Map.fromList [(O, 1), (H, -2)] 
    getFormula Methyl   = Just $ MolecularFormula $ Map.fromList [(C, 1), (H, 2)] 

instance MolecularFormulae Linkage where
    getFormula Acyl     = Just $ MolecularFormula $ Map.fromList [(O, 2), (H, -2)] 
    getFormula Alkyl    = Just $ MolecularFormula $ Map.fromList [(O, 1)] 
    getFormula Alkenyl  = Just $ MolecularFormula $ Map.fromList [(O, 1), (H, -2)] 

instance MolecularFormulae CarbonChain where
    getFormula (SimpleCarbonChain (Carbons c) db) =
        Just $ MolecularFormula $ Map.fromList [ (C, c)
        , (H, c * 2 + 1 - 2 * fromIntegral (length db)) ]
    getFormula (ComplexCarbonChain (Carbons c) db ms) =
        Just (MolecularFormula $ Map.fromList [ (C, c)
        , (H, c * 2 + 1 - 2 * fromIntegral (length db)) ])
        |+| (foldr1 (|+|) $ map (getFormula.moiety) ms)

instance MolecularFormulae CombinedChains where
    getFormula (CombinedChains (Carbons c) db) = 
        Just $ MolecularFormula $ Map.fromList [ (C, c)
        , (H, c * 2 + 2 - 2 * fromIntegral (length db)) ]

instance MolecularFormulae Radyl where
    getFormula (Radyl l r) =  getFormula l
                          |+| getFormula r

instance MolecularFormulae CombinedRadyls where
    getFormula (CombinedRadyls ls r) = getFormula r
                                    |+| (foldr1 (|+|) $ map getFormula ls)

find k v = Maybe.fromJust $ Map.lookup k v

lookupBlockComp :: String -> Maybe MolecularFormula 
lookupBlockComp n = find n blockComposition






