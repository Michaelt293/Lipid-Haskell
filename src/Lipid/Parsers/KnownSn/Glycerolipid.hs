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

dgP :: Parser (Radyl a) -> Parser (DG a)
dgP p = do
  _ <- string "DG "
  r1 <- p
  _ <- char '/'
  r2 <- p
  _ <- char '/'
  r3 <- p
  return $
    case (r1, r2, r3) of
       (r1, r2, Radyl _ (CarbonChain 0 [])) ->
         DG12 $ Glycerol r1 r2 GlycerolHydroxyl
       (r1, Radyl _ (CarbonChain 0 []), r3) ->
         DG13 $ Glycerol r1 GlycerolHydroxyl r3
       (Radyl _ (CarbonChain 0 []), r2, r3) ->
         DG23 $ Glycerol GlycerolHydroxyl r2 r3

dgDeltaP :: Parser (DG DeltaPosition)
dgDeltaP = dgP radylDeltaP

dgMaybeDeltaP :: Parser (DG (Maybe DeltaPosition))
dgMaybeDeltaP = dgP radylMaybeDeltaP

dgOmegaP :: Parser (DG OmegaPosition)
dgOmegaP = dgP radylOmegaP

dgMaybeOmegaP :: Parser (DG (Maybe OmegaPosition))
dgMaybeOmegaP = dgP radylMaybeOmegaP

mgP :: Parser (Radyl a) -> Parser (MG a)
mgP p = do
  _ <- string "MG "
  r1 <- p
  _ <- char '/'
  r2 <- p
  _ <- char '/'
  r3 <- p
  return $
    case (r1, r2, r3) of
       (r1, Radyl _ (CarbonChain 0 []), Radyl _ (CarbonChain 0 [])) ->
         MG1 $ Glycerol r1 GlycerolHydroxyl GlycerolHydroxyl
       (Radyl _ (CarbonChain 0 []), r2, Radyl _ (CarbonChain 0 [])) ->
         MG2 $ Glycerol GlycerolHydroxyl r2 GlycerolHydroxyl
       (Radyl _ (CarbonChain 0 []), Radyl _ (CarbonChain 0 []), r3) ->
         MG3 $ Glycerol GlycerolHydroxyl GlycerolHydroxyl r3

mgDeltaP :: Parser (MG DeltaPosition)
mgDeltaP = mgP radylDeltaP

mgMaybeDeltaP :: Parser (MG (Maybe DeltaPosition))
mgMaybeDeltaP = mgP radylMaybeDeltaP

mgOmegaP :: Parser (MG OmegaPosition)
mgOmegaP = mgP radylOmegaP

mgMaybeOmegaP :: Parser (MG (Maybe OmegaPosition))
mgMaybeOmegaP = mgP radylMaybeOmegaP
