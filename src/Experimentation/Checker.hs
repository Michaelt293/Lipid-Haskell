{-|
Module      : Experimentation.Checker
Description : Type checker
Copyright   : Michael Thomas
License     : GPL-3
aintainer   : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental
-}

{-# LANGUAGE FlexibleInstances #-}

module Experimentation.Checker where

import Control.Applicative
import System.Environment
import ElementIsotopes
import Lipid.Blocks hiding (linkage)
import Lipid.FattyAcid


class Checkable a where
  checker :: a -> Either String a

instance Checkable Carbons where
  checker chain@(Carbons n)
    | n < 0 = Left "The number of carbons of a carbon chain cannot be negative"
    | otherwise = Right chain

instance Checkable NumDoubleBonds where
  checker numBonds@(NumDoubleBonds n)
    | n < 0 = Left "The number of double bonds of a carbon chain cannot be negative"
    | otherwise = Right numBonds

instance Checkable Position where
  checker pos@(Omega n)
    | n < 1 = Left $ "The position of Omega " ++ show n ++ " is not valid"
    | otherwise = Right pos
  checker pos@(Delta n)
    | n < 2 = Left $ "The position of Delta " ++ show n ++ " is not valid"
    | otherwise = Right pos

instance Checkable (Maybe Position) where
  checker p =
    case p of
      Nothing -> Right Nothing
      (Just p') -> Just <$> checker p'

instance Checkable DoubleBond where
  checker (DoubleBond p g) = do
    p' <- checker p
    return $ DoubleBond p' g

instance Checkable CarbonChain where
  checker chain =
    case chain of
      (SimpleCarbonChain c dbs) -> do
        c' <- checker c
        dbs' <- mapM checker dbs
        let positions = map dbPosition dbs'
        let test = map (fmap (positionOnChain c')) positions
        case (Just False `elem` test) of
          True -> Left "Double bond position is not possible"
          False -> return $ SimpleCarbonChain c' dbs'

positionOnChain c db =
  case (c, db) of
    (Carbons n, Delta x) -> n - 1 > x
    (Carbons n, Omega x) -> n - 1 > x

instance Checkable FA where
  checker fa =
    case fa of
      (FA c) -> FA <$> checker c

evaluate e =
  case e of
    Left x -> error x
    Right l -> l

