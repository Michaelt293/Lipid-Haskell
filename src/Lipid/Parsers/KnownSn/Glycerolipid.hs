{-|
Module      : Lipid.Parsers.KnownSn.Glycerolipid
Description :
Copyright   : Michael Thomas
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Lipid.Parsers.KnownSn.Glycerolipid where

import Lipid.Parsers.Blocks
import Lipid.KnownSn.Glycerolipid
import Lipid.Blocks
import Text.Megaparsec
import Text.Megaparsec.String

tgP :: Parser (Radyl a) -> Parser (TG a)
tgP p = do
  _ <- string "TG "
  _sn1 <- p
  _ <- char '/'
  _sn2 <- p
  _ <- char '/'
  _sn3 <- p
  return . TG $ Glycerol {..}

tgDeltaP :: Parser (TG DeltaPosition)
tgDeltaP = tgP radylDeltaP

tgMaybeDeltaP :: Parser (TG (Maybe DeltaPosition))
tgMaybeDeltaP = tgP radylMaybeDeltaP

tgOmegaP :: Parser (TG OmegaPosition)
tgOmegaP = tgP radylOmegaP

tgMaybeOmegaP :: Parser (TG (Maybe OmegaPosition))
tgMaybeOmegaP = tgP radylMaybeOmegaP

-- dgP :: Parser (Radyl a) -> Parser (DG a)
-- dgP p = do
--   _ <- string "DG "
--   _sn1 <- p
--   _ <- char '/'
--   _sn2 <- p
--   _ <- char '/'
--   _sn3 <- p
--   return . DG $ Glycerol {..}
--
-- dgDeltaP :: Parser (DG DeltaPosition)
-- dgDeltaP = dgP radylDeltaP
--
-- dgMaybeDeltaP :: Parser (DG (Maybe DeltaPosition))
-- dgMaybeDeltaP = dgP radylMaybeDeltaP
--
-- dgOmegaP :: Parser (DG OmegaPosition)
-- dgOmegaP = dgP radylOmegaP
--
-- dgMaybeOmegaP :: Parser (DG (Maybe OmegaPosition))
-- dgMaybeOmegaP = dgP radylMaybeOmegaP
--
-- tgP :: Parser (Radyl a) -> Parser (TG a)
-- tgP p = do
--   _ <- string "TG "
--   _sn1 <- p
--   _ <- char '/'
--   _sn2 <- p
--   _ <- char '/'
--   _sn3 <- p
--   return . TG $ Glycerol {..}
--
-- tgDeltaP :: Parser (TG DeltaPosition)
-- tgDeltaP = tgP radylDeltaP
--
-- tgMaybeDeltaP :: Parser (TG (Maybe DeltaPosition))
-- tgMaybeDeltaP = tgP radylMaybeDeltaP
--
-- tgOmegaP :: Parser (TG OmegaPosition)
-- tgOmegaP = tgP radylOmegaP
--
-- tgMaybeOmegaP :: Parser (TG (Maybe OmegaPosition))
-- tgMaybeOmegaP = tgP radylMaybeOmegaP
