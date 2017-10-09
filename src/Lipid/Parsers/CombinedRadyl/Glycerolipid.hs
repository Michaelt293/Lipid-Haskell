{-|
Module      : Lipid.Parsers.CombinedRadyl.Glycerolipid
Description :
Copyright   : Michael Thomas
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Lipid.Parsers.CombinedRadyl.Glycerolipid where

import Lipid.Parsers.Blocks
import Lipid.CombinedRadyl.Glycerolipid
import Lipid.Blocks
import Data.Monoid ((<>))
import Text.Megaparsec
import Text.Megaparsec.String
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lift

tgP :: Parser a -> Parser (TG a)
tgP p = TG <$> (string "TG " *> threeCombinedRadylsP p)

tgMaybeP :: Parser (Maybe a) -> Parser (TG (Maybe a))
tgMaybeP p = TG <$> (string "TG " *> threeCombinedRadylsMaybeP p)

tgDeltaP :: Parser (TG DeltaPosition)
tgDeltaP = tgP deltaPositionP

tgMaybeDeltaP :: Parser (TG (Maybe DeltaPosition))
tgMaybeDeltaP = tgMaybeP maybeDeltaPositionP

tgOmegaP :: Parser (TG OmegaPosition)
tgOmegaP = tgP omegaPositionP

tgMaybeOmegaP :: Parser (TG (Maybe OmegaPosition))
tgMaybeOmegaP = tgMaybeP maybeOmegaPositionP

tgDelta :: QuasiQuoter
tgDelta = qQuoter tgDeltaP

tgMaybeDelta :: QuasiQuoter
tgMaybeDelta = qQuoter tgMaybeDeltaP

tgOmega :: QuasiQuoter
tgOmega = qQuoter tgOmegaP

tgMaybeOmega :: QuasiQuoter
tgMaybeOmega = qQuoter tgMaybeOmegaP

dgP :: Parser a -> Parser (DG a)
dgP p = DG <$> (string "DG " *> twoCombinedRadylsP p)

dgMaybeP :: Parser (Maybe a) -> Parser (DG (Maybe a))
dgMaybeP p = DG <$> (string "DG " *> twoCombinedRadylsMaybeP p)

dgDeltaP :: Parser (DG DeltaPosition)
dgDeltaP = dgP deltaPositionP

dgMaybeDeltaP :: Parser (DG (Maybe DeltaPosition))
dgMaybeDeltaP = dgMaybeP maybeDeltaPositionP

dgOmegaP :: Parser (DG OmegaPosition)
dgOmegaP = dgP omegaPositionP

dgMaybeOmegaP :: Parser (DG (Maybe OmegaPosition))
dgMaybeOmegaP = dgMaybeP maybeOmegaPositionP

dgDelta :: QuasiQuoter
dgDelta = qQuoter dgDeltaP

dgMaybeDelta :: QuasiQuoter
dgMaybeDelta = qQuoter dgMaybeDeltaP

dgOmega :: QuasiQuoter
dgOmega = qQuoter dgOmegaP

dgMaybeOmega :: QuasiQuoter
dgMaybeOmega = qQuoter dgMaybeOmegaP

$(deriveLift ''TG)

$(deriveLift ''DG)
