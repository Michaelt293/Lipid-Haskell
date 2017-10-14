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
import Text.Megaparsec
import Text.Megaparsec.String
import Language.Haskell.TH.Quote
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

dgP :: Parser (Radyl a) -> Parser (DG a)
dgP p = DG <$> (string "DG " *> twoRadylsP p)

dgDeltaP :: Parser (DG DeltaPosition)
dgDeltaP = dgP radylDeltaP

dgMaybeDeltaP :: Parser (DG (Maybe DeltaPosition))
dgMaybeDeltaP = dgP radylMaybeDeltaP

dgOmegaP :: Parser (DG OmegaPosition)
dgOmegaP = dgP radylOmegaP

dgMaybeOmegaP :: Parser (DG (Maybe OmegaPosition))
dgMaybeOmegaP = dgP radylMaybeOmegaP

dgDelta :: QuasiQuoter
dgDelta = qQuoter dgDeltaP

dgMaybeDelta :: QuasiQuoter
dgMaybeDelta = qQuoter dgMaybeDeltaP

dgOmega :: QuasiQuoter
dgOmega = qQuoter dgOmegaP

dgMaybeOmega :: QuasiQuoter
dgMaybeOmega = qQuoter dgMaybeOmegaP

mgP :: Parser (Radyl a) -> Parser (MG a)
mgP p = MG <$> (string "MG " *> p)

mgDeltaP :: Parser (MG DeltaPosition)
mgDeltaP = mgP radylDeltaP

mgMaybeDeltaP :: Parser (MG (Maybe DeltaPosition))
mgMaybeDeltaP = mgP radylMaybeDeltaP

mgOmegaP :: Parser (MG OmegaPosition)
mgOmegaP = mgP radylOmegaP

mgMaybeOmegaP :: Parser (MG (Maybe OmegaPosition))
mgMaybeOmegaP = mgP radylMaybeOmegaP

mgDelta :: QuasiQuoter
mgDelta = qQuoter mgDeltaP

mgMaybeDelta :: QuasiQuoter
mgMaybeDelta = qQuoter mgMaybeDeltaP

mgOmega :: QuasiQuoter
mgOmega = qQuoter mgOmegaP

mgMaybeOmega :: QuasiQuoter
mgMaybeOmega = qQuoter mgMaybeOmegaP

$(deriveLift ''TG)

$(deriveLift ''DG)

$(deriveLift ''MG)
