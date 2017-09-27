{-|
Module      : Lipid.ClassLevel.Glycerophospholipid
Description :
Copyright   : Michael Thomas
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental
-}
{-# LANGUAGE TemplateHaskell #-}

module Lipid.ClassLevel.Glycerophospholipid where

import Lipid.Blocks
import Control.Lens
import Data.Monoid ((<>))

newtype PA = PA
  { _getPA :: ClassLevel
  } deriving (Show, Eq, Ord)

makeClassy ''PA

instance Shorthand PA where
  shorthand (PA n) = "PA (" <> show n <> ")"

instance HasClassLevel PA where
  classLevel = getPA

newtype PE = PE
  { _getPE :: ClassLevel
  } deriving (Show, Eq, Ord)

makeClassy ''PE

instance Shorthand PE where
  shorthand (PE n) = "PE (" <> show n <> ")"

instance HasClassLevel PE where
  classLevel = getPE

newtype PC = PC
  { _getPC :: ClassLevel
  } deriving (Show, Eq, Ord)

makeClassy ''PC

instance Shorthand PC where
  shorthand (PC n) = "PC (" <> show n <> ")"

instance HasClassLevel PC where
  classLevel = getPC

newtype PG = PG
  { _getPG :: ClassLevel
  } deriving (Show, Eq, Ord)

makeClassy ''PG

instance Shorthand PG where
  shorthand (PG n) = "PG (" <> show n <> ")"

instance HasClassLevel PG where
  classLevel = getPG

newtype PGP = PGP
  { _getPGP :: ClassLevel
  } deriving (Show, Eq, Ord)

makeClassy ''PGP

instance Shorthand PGP where
  shorthand (PGP n) = "PGP (" <> show n <> ")"

instance HasClassLevel PGP where
  classLevel = getPGP

newtype PI = PI
  { _getPI :: ClassLevel
  } deriving (Show, Eq, Ord)

makeClassy ''PI

instance Shorthand PI where
  shorthand (PI n) = "PI (" <> show n <> ")"

instance HasClassLevel PI where
  classLevel = getPI

newtype PIP3 = PIP3
  { _getPIP3 :: ClassLevel
  } deriving (Show, Eq, Ord)

makeClassy ''PIP3

instance Shorthand PIP3 where
  shorthand (PIP3 n) = "PIP3 (" <> show n <> ")"

instance HasClassLevel PIP3 where
  classLevel = getPIP3

newtype PS = PS
  { _getPS :: ClassLevel
  } deriving (Show, Eq, Ord)

makeClassy ''PS

instance Shorthand PS where
  shorthand (PS n) = "PS (" <> show n <> ")"

instance HasClassLevel PS where
  classLevel = getPS

data PIP = PIP
  { _getHeadgroupPIP :: PhosphatidylinositolMonophosphate
  , _getPIP          :: ClassLevel
  } deriving (Show, Eq, Ord)

makeClassy ''PIP

instance Shorthand PIP where
  shorthand (PIP h n) = shorthand h <> " (" <> show n <> ")"

instance HasClassLevel PIP where
  classLevel = getPIP

data PIP2 = PIP2
  { _getHeadgroupPIP2 :: PhosphatidylinositolBisphosphate
  , _getPIP2          :: ClassLevel
  } deriving (Show, Eq, Ord)

makeClassy ''PIP2

instance Shorthand PIP2 where
  shorthand (PIP2 h n) = shorthand h <> " (" <> show n <> ")"

instance HasClassLevel PIP2 where
  classLevel = getPIP2

newtype CL = CL
  { _getCL :: ClassLevel
  } deriving (Show, Eq, Ord)

makeClassy ''CL

instance Shorthand CL where
  shorthand (CL n) = "CL (" <> show n <> ")"

instance HasClassLevel CL where
  classLevel = getCL
