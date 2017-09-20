{-|
Module      : Lipid.CombinedRadyl.Glycerophospholipid
Description :
Copyright   : Michael Thomas
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TemplateHaskell #-}

module Lipid.CombinedRadyl.Glycerophospholipid where

import Lipid.Blocks
import Lipid.Format
import qualified Lipid.KnownSn.Glycerophospholipid as KnownSn.Glycerophospholipid
import Control.Lens
import Data.Monoid ((<>))
