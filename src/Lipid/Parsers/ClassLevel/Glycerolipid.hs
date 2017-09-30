{-|
Module      :Lipid.Parsers.ClassLevel.Glycerolipid
Description :
Copyright   : Michael Thomas
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental
-}
{-# LANGUAGE TemplateHaskell #-}

module Lipid.Parsers.ClassLevel.Glycerolipid where

import Lipid.Blocks
import Control.Lens
import Data.Monoid ((<>))
import Lipid.ClassLevel.Glycerolipid
import Lipid.Parsers.Blocks
import Text.Megaparsec
import Text.Megaparsec.String
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lift

tgP :: Parser TG
tgP = do
  _ <- string "TG "
  TG <$> classLevelP

tg :: QuasiQuoter
tg = qQuoter tgP

dgP :: Parser DG
dgP = do
  _ <- string "DG "
  DG <$> classLevelP

dg :: QuasiQuoter
dg = qQuoter dgP

mgP :: Parser MG
mgP = do
  _ <- string "MG "
  MG <$> classLevelP

mg :: QuasiQuoter
mg = qQuoter mgP

$(deriveLift ''TG)

$(deriveLift ''DG)

$(deriveLift ''MG)
