{-|
Module      : Lipid.ClassLevel
Description :
Copyright   : Michael Thomas
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Lipid.ClassLevel where

import Control.Lens
import Data.Monoid ((<>))

newtype ClassLevel = ClassLevel
  { _getClassLevel :: Integer
  } deriving (Eq, Ord, Enum, Num)

makeClassy ''ClassLevel

instance Show ClassLevel where
  show (ClassLevel n) = show n
