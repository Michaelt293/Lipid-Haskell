{-|
Module      : Lipid.Parsers.CombinedRadyl.Glycerophospholipid
Description :
Copyright   : Michael Thomas
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Lipid.Parsers.CombinedRadyl.Glycerophospholipid where

import Lipid.Parsers.Blocks
import Lipid.CombinedRadyl.Glycerophospholipid
import Lipid.Blocks
import Text.Megaparsec
import Text.Megaparsec.String
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Lift

paP :: Parser a -> Parser (PA a)
paP p = PA <$> (string "PA " *> twoCombinedRadylsP p)

paDeltaP :: Parser (PA DeltaPosition)
paDeltaP = paP deltaPositionP

paMaybeDeltaP :: Parser (PA (Maybe DeltaPosition))
paMaybeDeltaP = paP maybeDeltaPositionP

paOmegaP :: Parser (PA OmegaPosition)
paOmegaP = paP omegaPositionP

paMaybeOmegaP :: Parser (PA (Maybe OmegaPosition))
paMaybeOmegaP = paP maybeOmegaPositionP

paDelta :: QuasiQuoter
paDelta = qQuoter paDeltaP

paMaybeDelta :: QuasiQuoter
paMaybeDelta = qQuoter paMaybeDeltaP

paOmega :: QuasiQuoter
paOmega = qQuoter paOmegaP

paMaybeOmega :: QuasiQuoter
paMaybeOmega = qQuoter paMaybeOmegaP


peP :: Parser a -> Parser (PE a)
peP p = PE <$> (string "PE " *> twoCombinedRadylsP p)

peDeltaP :: Parser (PE DeltaPosition)
peDeltaP = peP deltaPositionP

peMaybeDeltaP :: Parser (PE (Maybe DeltaPosition))
peMaybeDeltaP = peP maybeDeltaPositionP

peOmegaP :: Parser (PE OmegaPosition)
peOmegaP = peP omegaPositionP

peMaybeOmegaP :: Parser (PE (Maybe OmegaPosition))
peMaybeOmegaP = peP maybeOmegaPositionP

peDelta :: QuasiQuoter
peDelta = qQuoter peDeltaP

peMaybeDelta :: QuasiQuoter
peMaybeDelta = qQuoter peMaybeDeltaP

peOmega :: QuasiQuoter
peOmega = qQuoter peOmegaP

peMaybeOmega :: QuasiQuoter
peMaybeOmega = qQuoter peMaybeOmegaP

pcP :: Parser a -> Parser (PC a)
pcP p = PC <$> (string "PC " *> twoCombinedRadylsP p)

pcDeltaP :: Parser (PC DeltaPosition)
pcDeltaP = pcP deltaPositionP

pcMaybeDeltaP :: Parser (PC (Maybe DeltaPosition))
pcMaybeDeltaP = pcP maybeDeltaPositionP

pcOmegaP :: Parser (PC OmegaPosition)
pcOmegaP = pcP omegaPositionP

pcMaybeOmegaP :: Parser (PC (Maybe OmegaPosition))
pcMaybeOmegaP = pcP maybeOmegaPositionP

pcDelta :: QuasiQuoter
pcDelta = qQuoter pcDeltaP

pcMaybeDelta :: QuasiQuoter
pcMaybeDelta = qQuoter pcMaybeDeltaP

pcOmega :: QuasiQuoter
pcOmega = qQuoter pcOmegaP

pcMaybeOmega :: QuasiQuoter
pcMaybeOmega = qQuoter pcMaybeOmegaP

pgP :: Parser a -> Parser (PG a)
pgP p = PG <$> (string "PG " *> twoCombinedRadylsP p)

pgDeltaP :: Parser (PG DeltaPosition)
pgDeltaP = pgP deltaPositionP

pgMaybeDeltaP :: Parser (PG (Maybe DeltaPosition))
pgMaybeDeltaP = pgP maybeDeltaPositionP

pgOmegaP :: Parser (PG OmegaPosition)
pgOmegaP = pgP omegaPositionP

pgMaybeOmegaP :: Parser (PG (Maybe OmegaPosition))
pgMaybeOmegaP = pgP maybeOmegaPositionP

pgDelta :: QuasiQuoter
pgDelta = qQuoter pgDeltaP

pgMaybeDelta :: QuasiQuoter
pgMaybeDelta = qQuoter pgMaybeDeltaP

pgOmega :: QuasiQuoter
pgOmega = qQuoter pgOmegaP

pgMaybeOmega :: QuasiQuoter
pgMaybeOmega = qQuoter pgMaybeOmegaP

pgpP :: Parser a -> Parser (PGP a)
pgpP p = PGP <$> (string "PGP " *> twoCombinedRadylsP p)

pgpDeltaP :: Parser (PGP DeltaPosition)
pgpDeltaP = pgpP deltaPositionP

pgpMaybeDeltaP :: Parser (PGP (Maybe DeltaPosition))
pgpMaybeDeltaP = pgpP maybeDeltaPositionP

pgpOmegaP :: Parser (PGP OmegaPosition)
pgpOmegaP = pgpP omegaPositionP

pgpMaybeOmegaP :: Parser (PGP (Maybe OmegaPosition))
pgpMaybeOmegaP = pgpP maybeOmegaPositionP

pgpDelta :: QuasiQuoter
pgpDelta = qQuoter pgpDeltaP

pgpMaybeDelta :: QuasiQuoter
pgpMaybeDelta = qQuoter pgpMaybeDeltaP

pgpOmega :: QuasiQuoter
pgpOmega = qQuoter pgpOmegaP

pgpMaybeOmega :: QuasiQuoter
pgpMaybeOmega = qQuoter pgpMaybeOmegaP

psP :: Parser a -> Parser (PS a)
psP p = PS <$> (string "PS " *> twoCombinedRadylsP p)

psDeltaP :: Parser (PS DeltaPosition)
psDeltaP = psP deltaPositionP

psMaybeDeltaP :: Parser (PS (Maybe DeltaPosition))
psMaybeDeltaP = psP maybeDeltaPositionP

psOmegaP :: Parser (PS OmegaPosition)
psOmegaP = psP omegaPositionP

psMaybeOmegaP :: Parser (PS (Maybe OmegaPosition))
psMaybeOmegaP = psP maybeOmegaPositionP

psDelta :: QuasiQuoter
psDelta = qQuoter psDeltaP

psMaybeDelta :: QuasiQuoter
psMaybeDelta = qQuoter psMaybeDeltaP

psOmega :: QuasiQuoter
psOmega = qQuoter psOmegaP

psMaybeOmega :: QuasiQuoter
psMaybeOmega = qQuoter psMaybeOmegaP

piP :: Parser a -> Parser (PI a)
piP p = PI <$> (string "PI " *> twoCombinedRadylsP p)

piDeltaP :: Parser (PI DeltaPosition)
piDeltaP = piP deltaPositionP

piMaybeDeltaP :: Parser (PI (Maybe DeltaPosition))
piMaybeDeltaP = piP maybeDeltaPositionP

piOmegaP :: Parser (PI OmegaPosition)
piOmegaP = piP omegaPositionP

piMaybeOmegaP :: Parser (PI (Maybe OmegaPosition))
piMaybeOmegaP = piP maybeOmegaPositionP

piDelta :: QuasiQuoter
piDelta = qQuoter piDeltaP

piMaybeDelta :: QuasiQuoter
piMaybeDelta = qQuoter piMaybeDeltaP

piOmega :: QuasiQuoter
piOmega = qQuoter piOmegaP

piMaybeOmega :: QuasiQuoter
piMaybeOmega = qQuoter piMaybeOmegaP

pipP :: Parser a -> Parser (PIP a)
pipP p = do
  hg <- phosphatidylinositolMonophosphateP
  _ <- char ' '
  rs <- twoCombinedRadylsP p
  return $ PIP hg rs

pipDeltaP :: Parser (PIP DeltaPosition)
pipDeltaP = pipP deltaPositionP

pipMaybeDeltaP :: Parser (PIP (Maybe DeltaPosition))
pipMaybeDeltaP = pipP maybeDeltaPositionP

pipOmegaP :: Parser (PIP OmegaPosition)
pipOmegaP = pipP omegaPositionP

pipMaybeOmegaP :: Parser (PIP (Maybe OmegaPosition))
pipMaybeOmegaP = pipP maybeOmegaPositionP

pipDelta :: QuasiQuoter
pipDelta = qQuoter pipDeltaP

pipMaybeDelta :: QuasiQuoter
pipMaybeDelta = qQuoter pipMaybeDeltaP

pipOmega :: QuasiQuoter
pipOmega = qQuoter pipOmegaP

pipMaybeOmega :: QuasiQuoter
pipMaybeOmega = qQuoter pipMaybeOmegaP

pip2P :: Parser a -> Parser (PIP2 a)
pip2P p = do
  hg <- phosphatidylinositolBisphosphateP
  _ <- char ' '
  rs <- twoCombinedRadylsP p
  return $ PIP2 hg rs

pip2DeltaP :: Parser (PIP2 DeltaPosition)
pip2DeltaP = pip2P deltaPositionP

pip2MaybeDeltaP :: Parser (PIP2 (Maybe DeltaPosition))
pip2MaybeDeltaP = pip2P maybeDeltaPositionP

pip2OmegaP :: Parser (PIP2 OmegaPosition)
pip2OmegaP = pip2P omegaPositionP

pip2MaybeOmegaP :: Parser (PIP2 (Maybe OmegaPosition))
pip2MaybeOmegaP = pip2P maybeOmegaPositionP

pip2Delta :: QuasiQuoter
pip2Delta = qQuoter pip2DeltaP

pip2MaybeDelta :: QuasiQuoter
pip2MaybeDelta = qQuoter pip2MaybeDeltaP

pip2Omega :: QuasiQuoter
pip2Omega = qQuoter pip2OmegaP

pip2MaybeOmega :: QuasiQuoter
pip2MaybeOmega = qQuoter pip2MaybeOmegaP

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
