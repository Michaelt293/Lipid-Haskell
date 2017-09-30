{-|
Module      : Lipid.Parsers.Blocks
Description :
Copyright   : Michael Thomas
License     : GPL-3
aintainer   : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental
-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Lipid.Parsers.Blocks where

import Lipid.Blocks
import Control.Lens
import Control.Applicative
import Data.Monoid ((<>))
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lift


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

phosphatidicAcidP :: Parser PhosphatidicAcid
phosphatidicAcidP = string "PA" >> pure PhosphatidicAcid

phosphatidylethanolamineP :: Parser Phosphatidylethanolamine
phosphatidylethanolamineP = string "PE" >> pure Phosphatidylethanolamine

phosphatidylcholineP :: Parser Phosphatidylcholine
phosphatidylcholineP = string "PC" >> pure Phosphatidylcholine

phosphatidylglycerolP :: Parser Phosphatidylglycerol
phosphatidylglycerolP = string "PG" >> pure Phosphatidylglycerol

phosphatidylgylcerolphosphate :: Parser Phosphatidylgylcerolphosphate
phosphatidylgylcerolphosphate = string "PGP" >> pure Phosphatidylgylcerolphosphate

phosphatidylserineP :: Parser Phosphatidylserine
phosphatidylserineP = string "PS" >> pure Phosphatidylserine

phosphatidylinositolP :: Parser Phosphatidylinositol
phosphatidylinositolP = string "PI" >> pure Phosphatidylinositol

phosphatidylinositolMonophosphateP :: Parser PhosphatidylinositolMonophosphate
phosphatidylinositolMonophosphateP = do
  _ <- string "PIP["
  option PhosphatidylinositolMonophosphate $
    (string "5′]" >> pure Phosphatidylinositol5Phosphate) <|>
    (string "4′]" >> pure Phosphatidylinositol4Phosphate) <|>
    (string "3′]" >> pure Phosphatidylinositol3Phosphate)

phosphatidylinositolBisphosphateP :: Parser PhosphatidylinositolBisphosphate
phosphatidylinositolBisphosphateP = do
  _ <- string "PIP2["
  option PhosphatidylinositolBisphosphate $
    (string "3′,4′]" >> pure Phosphatidylinositol34Bisphosphate) <|>
    (string "3′,5′]" >> pure Phosphatidylinositol35Bisphosphate) <|>
    (string "4′,5′]" >> pure Phosphatidylinositol45Bisphosphate)

phosphatidylinositolTrisphosphateP :: Parser PhosphatidylinositolTrisphosphate
phosphatidylinositolTrisphosphateP =
  string "PIP3" >> pure PhosphatidylinositolTrisphosphate

-- Helper function used in QuasiQuoters
notHandled :: String -> a
notHandled feature =
  error $ feature <> " are not handled by the quasiquoter."

quoteHelper :: Lift a => Parser a -> String -> Q Exp
quoteHelper p s =
  case parse (p <* eof) "" s of
    Left err -> fail $
      "Could not parse lipid!\n" <> parseErrorPretty err
    Right v  -> lift v

qQuoter :: Lift a => Parser a -> QuasiQuoter
qQuoter p = QuasiQuoter
  { quoteExp  = quoteHelper p
  , quotePat  = notHandled "patterns"
  , quoteType = notHandled "types"
  , quoteDec  = notHandled "declarations"
  }


$(deriveLift ''DeltaPosition)

$(deriveLift ''OmegaPosition)

$(deriveLift ''ClassLevel)

$(deriveLift ''Glycerol)

$(deriveLift ''Radyl)

$(deriveLift ''CarbonChain)

$(deriveLift ''Linkage)

$(deriveLift ''DoubleBond)

$(deriveLift ''NumCarbons)

$(deriveLift ''Geometry)

$(deriveLift ''GlycerolHydroxyl)

$(deriveLift ''PhosphatidicAcid)

$(deriveLift ''Phosphatidylethanolamine)

$(deriveLift ''Phosphatidylglycerol)

$(deriveLift ''Phosphatidylcholine)

$(deriveLift ''Phosphatidylgylcerolphosphate)

$(deriveLift ''Phosphatidylserine)

$(deriveLift ''Phosphatidylinositol)

$(deriveLift ''PhosphatidylinositolMonophosphate)

$(deriveLift ''PhosphatidylinositolBisphosphate)

$(deriveLift ''PhosphatidylinositolTrisphosphate)
