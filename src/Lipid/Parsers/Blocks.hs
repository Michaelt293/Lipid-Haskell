{-|
Module      : Lipid.Parsers.Blocks
Description :
Copyright   : Michael Thomas
License     : GPL-3
aintainer   : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Lipid.Parsers.Blocks where

import Lipid.Blocks
import Control.Lens
import Control.Applicative
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L


numCarbonsP :: Parser NumCarbons
numCarbonsP = NumCarbons <$> L.integer

numDoubleBondsP :: Parser NumDoubleBonds
numDoubleBondsP = NumDoubleBonds <$> L.integer

deltaPositionP :: Parser DeltaPosition
deltaPositionP = DeltaPosition <$> L.integer

maybeDeltaPositionP :: Parser (Maybe DeltaPosition)
maybeDeltaPositionP =
  (char '?' >> pure Nothing) <|> (Just <$> deltaPositionP)

omegaPositionP :: Parser OmegaPosition
omegaPositionP = string "n-" *> (OmegaPosition <$> L.integer)

maybeOmegaPositionP :: Parser (Maybe OmegaPosition)
maybeOmegaPositionP =
  (char '?' >> return Nothing) <|> (Just <$> omegaPositionP)

geometryP :: Parser Geometry
geometryP = (char 'Z' >> pure Cis) <|> (char 'E' >> pure Trans)

doubleBondWithGeometryP :: Parser a -> Parser (DoubleBond a)
doubleBondWithGeometryP p = do
  pos <- p
  geo <- optional geometryP
  return $ DoubleBond pos geo

doubleBondP :: Parser a -> Parser (DoubleBond a)
doubleBondP p = (`DoubleBond` Nothing) <$> p

carbonChainDeltaP :: Parser (CarbonChain DeltaPosition)
carbonChainDeltaP = do
  numCs <- numCarbonsP
  _ <- char ':'
  _ <- numDoubleBondsP -- error "Number of double bonds does not equal the number of double bonds provided"
  doublebonds <- list $ doubleBondWithGeometryP deltaPositionP
  return $ CarbonChain numCs doublebonds

carbonChainMaybeDeltaP :: Parser (CarbonChain (Maybe DeltaPosition))
carbonChainMaybeDeltaP = do
  numCs <- numCarbonsP
  _ <- char ':'
  numDb <- numDoubleBondsP
  doublebonds <- optional . list $ doubleBondWithGeometryP maybeDeltaPositionP
  return $
    case doublebonds of
      Nothing -> CarbonChain numCs $
        replicate (fromIntegral numDb) (DoubleBond Nothing Nothing)
      Just dbs -> CarbonChain numCs dbs

carbonChainOmegaP :: Parser (CarbonChain OmegaPosition)
carbonChainOmegaP = do
  numCs <- numCarbonsP
  _ <- char ':'
  numDb <- numDoubleBondsP -- error "Number of double bonds does not equal the number of double bonds provided"
  doublebonds <- list $ doubleBondP omegaPositionP
  return $
    if | length doublebonds == fromIntegral numDb ->
           CarbonChain numCs doublebonds
       | length doublebonds == 1 ->
           CarbonChain numCs .
             take (fromIntegral numDb) $
             (`DoubleBond` Nothing) <$> iterate (+3) (head doublebonds^.dbPosition)
       | otherwise ->
           error "Number of double bonds does not equal the number of double bonds provided"

carbonChainMaybeOmegaP :: Parser (CarbonChain (Maybe OmegaPosition))
carbonChainMaybeOmegaP = do
  numCs <- numCarbonsP
  _ <- char ':'
  numDb <- numDoubleBondsP
  doublebonds <- optional . list $ doubleBondP maybeOmegaPositionP
  return $
    case doublebonds of
      Nothing -> CarbonChain numCs $
        replicate (fromIntegral numDb) (DoubleBond Nothing Nothing)
      Just dbs -> CarbonChain numCs dbs

-- adapted from CIS194 Monads II lecture
list :: Parser a -> Parser [a]
list p = char '(' *> sepBy p comma <* char ')'
    where comma = many (char ' ') *> char ',' <* many (char ' ')

classLevelP :: Parser ClassLevel
classLevelP =
  ClassLevel <$> (char '(' *> L.integer <* char ')')

linkageP :: Parser Linkage
linkageP = do
  oOrP <- optional $ string "O-" <|> string "P-"
  case oOrP of
    (Just "O-") -> pure Alkyl
    (Just "P-") -> pure Alkenyl
    _ -> pure Acyl

radylDeltaP :: Parser (Radyl DeltaPosition)
radylDeltaP = do
  link <- linkageP
  chain <- carbonChainDeltaP
  return $ Radyl link chain

radylMaybeDeltaP :: Parser (Radyl (Maybe DeltaPosition))
radylMaybeDeltaP = do
  link <- linkageP
  chain <- carbonChainMaybeDeltaP
  return $ Radyl link chain

radylOmegaP :: Parser (Radyl OmegaPosition)
radylOmegaP = do
  link <- linkageP
  chain <- carbonChainOmegaP
  return $ Radyl link chain

radylMaybeOmegaP :: Parser (Radyl (Maybe OmegaPosition))
radylMaybeOmegaP = do
  link <- linkageP
  chain <- carbonChainMaybeOmegaP
  return $ Radyl link chain

glycerolHydroxylP :: Parser GlycerolHydroxyl
glycerolHydroxylP = string "0:0" >> pure GlycerolHydroxyl
