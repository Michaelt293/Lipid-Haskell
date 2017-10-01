{-|
Module      : Lipid.Parsers.UnknownSn.Glycerolipid
Description :
Copyright   : Michael Thomas
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Lipid.Parsers.UnknownSn.Glycerolipid where

import Lipid.Parsers.Blocks
import Lipid.UnknownSn.Glycerolipid
import Lipid.Blocks
import Data.Monoid ((<>))
import Text.Megaparsec
import Text.Megaparsec.String
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lift

tgP :: Parser (Radyl a) -> Parser (TG a)
tgP p = TG <$> (string "TG " *> threeRadylsP p)

tgDeltaP :: Parser (TG DeltaPosition)
tgDeltaP = tgP radylDeltaP

tgMaybeDeltaP :: Parser (TG (Maybe DeltaPosition))
tgMaybeDeltaP = tgP radylMaybeDeltaP

tgOmegaP :: Parser (TG OmegaPosition)
tgOmegaP = tgP radylOmegaP

tgMaybeOmegaP :: Parser (TG (Maybe OmegaPosition))
tgMaybeOmegaP = tgP radylMaybeOmegaP

tgDelta :: QuasiQuoter
tgDelta = qQuoter tgDeltaP

tgMaybeDelta :: QuasiQuoter
tgMaybeDelta = qQuoter tgMaybeDeltaP

tgOmega :: QuasiQuoter
tgOmega = qQuoter tgOmegaP

tgMaybeOmega :: QuasiQuoter
tgMaybeOmega = qQuoter tgMaybeOmegaP

$(deriveLift ''TG)

$(deriveLift ''DG)

$(deriveLift ''MG)
