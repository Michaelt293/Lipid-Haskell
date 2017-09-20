{-|
Module      : Lipid.ClassLevel.Glycerolipid
Description :
Copyright   : Michael Thomas
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Lipid.ClassLevel.Glycerolipid where

import Lipid.Blocks
import Control.Lens
import Data.Monoid ((<>))

newtype ClassLevel = ClassLevel
  { _getClassLevel :: Integer
  } deriving (Eq, Ord, Enum, Num)

makeClassy ''ClassLevel

instance Show ClassLevel where
  show (ClassLevel n) = show n

newtype TG = TG
 { _getTG :: ClassLevel
 } deriving (Show, Eq, Ord)

makeClassy ''TG

instance Shorthand TG where
  shorthand (TG n) = "MG (" <> show n <> ")"

instance HasClassLevel TG where
   classLevel = getTG

newtype DG = DG
 { _getDG :: ClassLevel
 } deriving (Show, Eq, Ord)

makeClassy ''DG

instance Shorthand DG where
  shorthand (DG n) = "DG (" <> show n <> ")"

instance HasClassLevel DG where
   classLevel = getDG

newtype MG = MG
 { _getMG :: ClassLevel
 } deriving (Show, Eq, Ord)

makeClassy ''MG

instance Shorthand MG where
  shorthand (MG n) = "MG (" <> show n <> ")"

instance HasClassLevel MG where
   classLevel = getMG 