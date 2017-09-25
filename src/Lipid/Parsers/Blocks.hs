{-|
Module      : Lipid.Parsers.Blocks
Description :
Copyright   : Michael Thomas
License     : GPL-3
aintainer   : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental
-}

{-# LANGUAGE OverloadedStrings #-}

module Lipid.Parsers.Blocks where

import Lipid.Blocks
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

maybeDeltaPosition :: Parser (Maybe DeltaPosition)
maybeDeltaPosition =
  (char '?' >> return Nothing) <|> (Just <$> deltaPositionP)

omegaPositionP :: Parser OmegaPosition
omegaPositionP = string "n-" *> (OmegaPosition <$> L.integer)

maybeOmegaPosition :: Parser (Maybe OmegaPosition)
maybeOmegaPosition =
  (char '?' >> return Nothing) <|> (Just <$> omegaPositionP)

geometryP :: Parser Geometry
geometryP = (char 'Z' >> return Cis) <|> (char 'E' >> return Trans)

doubleBondP :: Parser a -> Parser (DoubleBond a)
doubleBondP p = do
  pos <- p
  geo <- optional geometryP
  return $ DoubleBond pos geo

carbonChainP :: Parser a -> Parser (CarbonChain a)
carbonChainP p = do
  numCs <- numCarbonsP
  _ <- char ':'
  numDb <- numDoubleBondsP -- error "Number of double bonds does not equal the number of double bonds provided"
  doublebonds <- optional . list $ doubleBondP p
  return $
    case doublebonds of
      Nothing -> CarbonChain numCs [] -- fix
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
    (Just "O-") -> return Alkyl
    (Just "P-") -> return Alkenyl
    _ -> return Acyl

radylP :: Parser a -> Parser (Radyl a)
radylP p = do
  link <- linkageP
  chain <- carbonChainP p
  return $ Radyl link chain
