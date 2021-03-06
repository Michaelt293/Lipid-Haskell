{-|
Module      : Lipid.Parsers.KnownSn.Glycerophospholipid
Description :
Copyright   : Michael Thomas
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Lipid.Parsers.KnownSn.Glycerophospholipid where

import Lipid.Parsers.Blocks
import Lipid.KnownSn.Glycerophospholipid
import Lipid.Blocks
import Text.Megaparsec
import Text.Megaparsec.String
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Lift

glyceroPhospholipidP :: Parser a -> Parser b -> Parser (Glycerol b b a)
glyceroPhospholipidP hp rp = do
  headgroup <- hp
  _ <- many $ char ' '
  r1 <- rp
  _ <- char '/'
  r2 <- rp
  return $ Glycerol r1 r2 headgroup

paP :: Parser (Radyl a) -> Parser (PA a)
paP p = PA <$> glyceroPhospholipidP phosphatidicAcidP p

paDeltaP :: Parser (PA DeltaPosition)
paDeltaP = paP radylDeltaP

paMaybeDeltaP :: Parser (PA (Maybe DeltaPosition))
paMaybeDeltaP = paP radylMaybeDeltaP

paOmegaP :: Parser (PA OmegaPosition)
paOmegaP = paP radylOmegaP

paMaybeOmegaP :: Parser (PA (Maybe OmegaPosition))
paMaybeOmegaP = paP radylMaybeOmegaP

paDelta :: QuasiQuoter
paDelta = qQuoter paDeltaP

paMaybeDelta :: QuasiQuoter
paMaybeDelta = qQuoter paMaybeDeltaP

paOmega :: QuasiQuoter
paOmega = qQuoter paOmegaP

paMaybeOmega :: QuasiQuoter
paMaybeOmega = qQuoter paMaybeOmegaP

peP :: Parser (Radyl a) -> Parser (PE a)
peP p = PE <$> glyceroPhospholipidP phosphatidylethanolamineP p

peDeltaP :: Parser (PE DeltaPosition)
peDeltaP = peP radylDeltaP

peMaybeDeltaP :: Parser (PE (Maybe DeltaPosition))
peMaybeDeltaP = peP radylMaybeDeltaP

peOmegaP :: Parser (PE OmegaPosition)
peOmegaP = peP radylOmegaP

peMaybeOmegaP :: Parser (PE (Maybe OmegaPosition))
peMaybeOmegaP = peP radylMaybeOmegaP

peDelta :: QuasiQuoter
peDelta = qQuoter peDeltaP

peMaybeDelta :: QuasiQuoter
peMaybeDelta = qQuoter peMaybeDeltaP

peOmega :: QuasiQuoter
peOmega = qQuoter peOmegaP

peMaybeOmega :: QuasiQuoter
peMaybeOmega = qQuoter peMaybeOmegaP

pcP :: Parser (Radyl a) -> Parser (PC a)
pcP p = PC <$> glyceroPhospholipidP phosphatidylcholineP p

pcDeltaP :: Parser (PC DeltaPosition)
pcDeltaP = pcP radylDeltaP

pcMaybeDeltaP :: Parser (PC (Maybe DeltaPosition))
pcMaybeDeltaP = pcP radylMaybeDeltaP

pcOmegaP :: Parser (PC OmegaPosition)
pcOmegaP = pcP radylOmegaP

pcMaybeOmegaP :: Parser (PC (Maybe OmegaPosition))
pcMaybeOmegaP = pcP radylMaybeOmegaP

pcDelta :: QuasiQuoter
pcDelta = qQuoter pcDeltaP

pcMaybeDelta :: QuasiQuoter
pcMaybeDelta = qQuoter pcMaybeDeltaP

pcOmega :: QuasiQuoter
pcOmega = qQuoter pcOmegaP

pcMaybeOmega :: QuasiQuoter
pcMaybeOmega = qQuoter pcMaybeOmegaP

pgP :: Parser (Radyl a) -> Parser (PG a)
pgP p = PG <$> glyceroPhospholipidP phosphatidylglycerolP p

pgDeltaP :: Parser (PG DeltaPosition)
pgDeltaP = pgP radylDeltaP

pgMaybeDeltaP :: Parser (PG (Maybe DeltaPosition))
pgMaybeDeltaP = pgP radylMaybeDeltaP

pgOmegaP :: Parser (PG OmegaPosition)
pgOmegaP = pgP radylOmegaP

pgMaybeOmegaP :: Parser (PG (Maybe OmegaPosition))
pgMaybeOmegaP = pgP radylMaybeOmegaP

pgDelta :: QuasiQuoter
pgDelta = qQuoter pgDeltaP

pgMaybeDelta :: QuasiQuoter
pgMaybeDelta = qQuoter pgMaybeDeltaP

pgOmega :: QuasiQuoter
pgOmega = qQuoter pgOmegaP

pgMaybeOmega :: QuasiQuoter
pgMaybeOmega = qQuoter pgMaybeOmegaP

pgpP :: Parser (Radyl a) -> Parser (PGP a)
pgpP p = PGP <$> glyceroPhospholipidP phosphatidylgylcerolphosphateP p

pgpDeltaP :: Parser (PGP DeltaPosition)
pgpDeltaP = pgpP radylDeltaP

pgpMaybeDeltaP :: Parser (PGP (Maybe DeltaPosition))
pgpMaybeDeltaP = pgpP radylMaybeDeltaP

pgpOmegaP :: Parser (PGP OmegaPosition)
pgpOmegaP = pgpP radylOmegaP

pgpMaybeOmegaP :: Parser (PGP (Maybe OmegaPosition))
pgpMaybeOmegaP = pgpP radylMaybeOmegaP

pgpDelta :: QuasiQuoter
pgpDelta = qQuoter pgpDeltaP

pgpMaybeDelta :: QuasiQuoter
pgpMaybeDelta = qQuoter pgpMaybeDeltaP

pgpOmega :: QuasiQuoter
pgpOmega = qQuoter pgpOmegaP

pgpMaybeOmega :: QuasiQuoter
pgpMaybeOmega = qQuoter pgpMaybeOmegaP

psP :: Parser (Radyl a) -> Parser (PS a)
psP p = PS <$> glyceroPhospholipidP phosphatidylserineP p

psDeltaP :: Parser (PS DeltaPosition)
psDeltaP = psP radylDeltaP

psMaybeDeltaP :: Parser (PS (Maybe DeltaPosition))
psMaybeDeltaP = psP radylMaybeDeltaP

psOmegaP :: Parser (PS OmegaPosition)
psOmegaP = psP radylOmegaP

psMaybeOmegaP :: Parser (PS (Maybe OmegaPosition))
psMaybeOmegaP = psP radylMaybeOmegaP

psDelta :: QuasiQuoter
psDelta = qQuoter psDeltaP

psMaybeDelta :: QuasiQuoter
psMaybeDelta = qQuoter psMaybeDeltaP

psOmega :: QuasiQuoter
psOmega = qQuoter psOmegaP

psMaybeOmega :: QuasiQuoter
psMaybeOmega = qQuoter psMaybeOmegaP

piP :: Parser (Radyl a) -> Parser (PI a)
piP p = PI <$> glyceroPhospholipidP phosphatidylinositolP p

piDeltaP :: Parser (PI DeltaPosition)
piDeltaP = piP radylDeltaP

piMaybeDeltaP :: Parser (PI (Maybe DeltaPosition))
piMaybeDeltaP = piP radylMaybeDeltaP

piOmegaP :: Parser (PI OmegaPosition)
piOmegaP = piP radylOmegaP

piMaybeOmegaP :: Parser (PI (Maybe OmegaPosition))
piMaybeOmegaP = piP radylMaybeOmegaP

piDelta :: QuasiQuoter
piDelta = qQuoter piDeltaP

piMaybeDelta :: QuasiQuoter
piMaybeDelta = qQuoter piMaybeDeltaP

piOmega :: QuasiQuoter
piOmega = qQuoter piOmegaP

piMaybeOmega :: QuasiQuoter
piMaybeOmega = qQuoter piMaybeOmegaP

pipP :: Parser (Radyl a) -> Parser (PIP a)
pipP p = PIP <$> glyceroPhospholipidP phosphatidylinositolMonophosphateP p

pipDeltaP :: Parser (PIP DeltaPosition)
pipDeltaP = pipP radylDeltaP

pipMaybeDeltaP :: Parser (PIP (Maybe DeltaPosition))
pipMaybeDeltaP = pipP radylMaybeDeltaP

pipOmegaP :: Parser (PIP OmegaPosition)
pipOmegaP = pipP radylOmegaP

pipMaybeOmegaP :: Parser (PIP (Maybe OmegaPosition))
pipMaybeOmegaP = pipP radylMaybeOmegaP

pipDelta :: QuasiQuoter
pipDelta = qQuoter pipDeltaP

pipMaybeDelta :: QuasiQuoter
pipMaybeDelta = qQuoter pipMaybeDeltaP

pipOmega :: QuasiQuoter
pipOmega = qQuoter pipOmegaP

pipMaybeOmega :: QuasiQuoter
pipMaybeOmega = qQuoter pipMaybeOmegaP

pip2P :: Parser (Radyl a) -> Parser (PIP2 a)
pip2P p = PIP2 <$> glyceroPhospholipidP phosphatidylinositolBisphosphateP p

pip2DeltaP :: Parser (PIP2 DeltaPosition)
pip2DeltaP = pip2P radylDeltaP

pip2MaybeDeltaP :: Parser (PIP2 (Maybe DeltaPosition))
pip2MaybeDeltaP = pip2P radylMaybeDeltaP

pip2OmegaP :: Parser (PIP2 OmegaPosition)
pip2OmegaP = pip2P radylOmegaP

pip2MaybeOmegaP :: Parser (PIP2 (Maybe OmegaPosition))
pip2MaybeOmegaP = pip2P radylMaybeOmegaP

pip2Delta :: QuasiQuoter
pip2Delta = qQuoter pip2DeltaP

pip2MaybeDelta :: QuasiQuoter
pip2MaybeDelta = qQuoter pip2MaybeDeltaP

pip2Omega :: QuasiQuoter
pip2Omega = qQuoter pip2OmegaP

pip2MaybeOmega :: QuasiQuoter
pip2MaybeOmega = qQuoter pip2MaybeOmegaP

pip3P :: Parser (Radyl a) -> Parser (PIP3 a)
pip3P p = PIP3 <$> glyceroPhospholipidP phosphatidylinositolTrisphosphateP p

pip3DeltaP :: Parser (PIP3 DeltaPosition)
pip3DeltaP = pip3P radylDeltaP

pip3MaybeDeltaP :: Parser (PIP3 (Maybe DeltaPosition))
pip3MaybeDeltaP = pip3P radylMaybeDeltaP

pip3OmegaP :: Parser (PIP3 OmegaPosition)
pip3OmegaP = pip3P radylOmegaP

pip3MaybeOmegaP :: Parser (PIP3 (Maybe OmegaPosition))
pip3MaybeOmegaP = pip3P radylMaybeOmegaP

pip3Delta :: QuasiQuoter
pip3Delta = qQuoter pip3DeltaP

pip3MaybeDelta :: QuasiQuoter
pip3MaybeDelta = qQuoter pip3MaybeDeltaP

pip3Omega :: QuasiQuoter
pip3Omega = qQuoter pip3OmegaP

pip3MaybeOmega :: QuasiQuoter
pip3MaybeOmega = qQuoter pip3MaybeOmegaP

-- clP :: Parser (Radyl a) -> Parser (CL a)
-- clP p = do
--   _ <- string "CL "
--   r1 <- p
--   _ <- char '/'
--   r2 <- p
--   _ <- char '/'
--   r3 <- p
--   _ <- char '/'
--   r4 <- p
--   return $ (Cardiolipin r3 r4)

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
