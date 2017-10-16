{-|
Module      :Lipid.Parsers.FattyAcid
Description :
Copyright   : Michael Thomas
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental
-}
{-# LANGUAGE TemplateHaskell #-}

module Lipid.Parsers.FattyAcid where

import Lipid.Blocks
import Control.Lens
import Data.Monoid ((<>))
import Lipid.FattyAcid
import Lipid.Parsers.Blocks
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lift

faP :: Parser (CarbonChain a) -> Parser (FA a)
faP p =
  string "FA " *> ((ClassLevelFA <$> classLevelP) <|> (FA <$> p))

faDeltaP :: Parser (FA DeltaPosition)
faDeltaP = faP carbonChainDeltaP

faMaybeDeltaP :: Parser (FA (Maybe DeltaPosition))
faMaybeDeltaP = faP carbonChainMaybeDeltaP

faOmegaP :: Parser (FA OmegaPosition)
faOmegaP = faP carbonChainOmegaP

faMaybeOmegaP :: Parser (FA (Maybe OmegaPosition))
faMaybeOmegaP = faP carbonChainMaybeOmegaP

faDelta :: QuasiQuoter
faDelta = qQuoter faDeltaP

faMaybeDelta :: QuasiQuoter
faMaybeDelta = qQuoter faMaybeDeltaP

faOmega :: QuasiQuoter
faOmega = qQuoter faOmegaP

faMaybeOmega :: QuasiQuoter
faMaybeOmega = qQuoter faMaybeOmegaP

$(deriveLift ''FA)
