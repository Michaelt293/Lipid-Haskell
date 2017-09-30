{-|
Module      : Lipid.Parsers.ClassLevel.Glycerophospholipid
Description :
Copyright   : Michael Thomas
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental
-}
{-# LANGUAGE TemplateHaskell #-}

module Lipid.Parsers.ClassLevel.Glycerophospholipid where

import Lipid.Blocks
import Control.Lens
import Data.Monoid ((<>))
import Lipid.ClassLevel.Glycerophospholipid
import Lipid.Parsers.Blocks
import Text.Megaparsec
import Text.Megaparsec.String
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lift

paP :: Parser PA
paP = do
  _ <- string "PA "
  PA <$> classLevelP

pa :: QuasiQuoter
pa = qQuoter paP

peP :: Parser PE
peP = do
  _ <- string "PE "
  PE <$> classLevelP

pe :: QuasiQuoter
pe = qQuoter peP

pcP :: Parser PC
pcP = do
  _ <- string "PC "
  PC <$> classLevelP

pc :: QuasiQuoter
pc = qQuoter pcP

pgP :: Parser PG
pgP = do
  _ <- string "PG "
  PG <$> classLevelP

pg :: QuasiQuoter
pg = qQuoter pgP

pgpP :: Parser PGP
pgpP = do
  _ <- string "PGP "
  PGP <$> classLevelP

pgp :: QuasiQuoter
pgp = qQuoter pgpP

psP :: Parser PS
psP = do
  _ <- string "PS "
  PS <$> classLevelP

ps :: QuasiQuoter
ps = qQuoter psP

piP :: Parser PI
piP = do
  _ <- string "PI "
  PI <$> classLevelP

pi :: QuasiQuoter
pi = qQuoter piP

pipP :: Parser PIP
pipP = do
  hg <- phosphatidylinositolMonophosphateP
  _ <- char ' '
  PIP <$> pure hg <*> classLevelP

pip :: QuasiQuoter
pip = qQuoter pipP

pip2P :: Parser PIP2
pip2P = do
  hg <- phosphatidylinositolBisphosphateP
  _ <- char ' '
  PIP2 <$> pure hg <*> classLevelP

pip2 :: QuasiQuoter
pip2 = qQuoter pip2P

pip3P :: Parser PIP3
pip3P = do
  _ <- string "PIP3 "
  PIP3 <$> classLevelP

pip3 :: QuasiQuoter
pip3 = qQuoter pip3P

$(deriveLift ''PA)

$(deriveLift ''PE)

$(deriveLift ''PC)

$(deriveLift ''PG)

$(deriveLift ''PGP)

$(deriveLift ''PS)

$(deriveLift ''PI)

$(deriveLift ''PIP)

$(deriveLift ''PIP2)

$(deriveLift ''PIP3)
