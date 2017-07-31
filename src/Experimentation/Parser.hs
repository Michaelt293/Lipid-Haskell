{-|
Module      : Experimentation.Parser
Description : Lipid name parser
Copyright   : Michael Thomas
License     : GPL-3
aintainer   : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}

module Experimentation.Parser where

import System.Environment
import ElementIsotopes
import Lipid.Blocks hiding (linkage)
import Lipid.FattyAcid
import Lipid.Glycerophospholipid
import Experimentation.Checker
import Data.Attoparsec.Char8
import Data.Word
import Control.Applicative
import Data.Maybe (fromJust)


numCarbon :: Parser Carbons
numCarbon = Carbons . read <$> many1 digit

numDb :: Parser NumDoubleBonds
numDb = NumDoubleBonds . read <$> many1 digit

delta :: Parser Position
delta = Delta . read <$> many1 digit

omega :: Parser Position
omega = string "n-" *> (Omega . read <$> many1 digit)

dbGeometry :: Parser Geometry
dbGeometry =  (char 'Z' >> return Cis)
          <|> (char 'E' >> return Trans)

omegaDbPosition :: Parser DoubleBond
omegaDbPosition = do
  position <- omega
  return $ DoubleBond (Just position) Nothing

deltaDbPosition :: Parser DoubleBond
deltaDbPosition = do
  position <- optional delta
  geometry <- optional dbGeometry
  case (position, geometry) of
    (Nothing, Nothing) -> return $ DoubleBond Nothing  Nothing
    (Just p, Nothing)  -> return $ DoubleBond position Nothing
    (Nothing, Just g)  -> return $ DoubleBond Nothing  geometry
    (Just p, Just g)   -> return $ DoubleBond position geometry

doubleBond =  omegaDbPosition
          <|> deltaDbPosition

-- adapted from CIS194 Monads II lecture
list :: Parser a -> Parser [a]
list p = char '(' *> sepBy p comma <* char ')'
    where comma = many (char ' ') *> char ',' <* many (char ' ')

doubleBondList :: Parser [DoubleBond]
doubleBondList = list doubleBond

integerMass :: Parser IntegerMass
integerMass = do
  x <- char '(' *> many1 digit <* char ')'
  return $ read x

snPosition :: Parser Char
snPosition = char '/' <|> char '_'

linkageParser :: Parser Linkage
linkageParser = do
  oOrP <- optional $ string "O-" <|> string "P-"
  case oOrP of
    Just "O-" -> return Alkyl
    Just "P-" -> return Alkenyl
    _ -> return Acyl

simpleChain = do
  carbons <- numCarbon
  char ':'
  noOfdbs <- numDb
  dbs <- optional doubleBondList
  case dbs of
    Nothing -> return . SimpleCarbonChain carbons $
                        replicateBonds noOfdbs unknownDb
      where unknownDb = DoubleBond Nothing Nothing
            replicateBonds n = replicate (fromIntegral $ numDoubleBonds n)
    (Just dbs') -> if length dbs' /= (fromIntegral $ numDoubleBonds noOfdbs)
                       then error "Number of double bonds does not equal the number of double bonds provided"
                       else return $ SimpleCarbonChain carbons dbs'


-- need a series of helper functions

radylParser = do
  link <- linkageParser
  chain <- simpleChain
  return $ Radyl link chain


faParser = do
  string "FA "
  intMass <- optional integerMass
  case intMass of
    (Just intMass') -> return . ParsedFA $ ClassLevelFA intMass'
    _ -> do
      chain' <- simpleChain
      return . ParsedFA $ FA chain'

paParser = do
  string "PA "
  r1 <- radylParser
  identifier <- snPosition
  r2 <- radylParser
  case identifier of
    '/' -> return . ParsedPA $ KnownSnPA r1 r2
    '_' -> return . ParsedPA $ UnknownSnPA r1 r2


data ParsedLipid a where
  ParsedFA :: FA -> ParsedLipid FA
  ParsedPA :: PA -> ParsedLipid PA

eval :: ParsedLipid a -> a
eval (ParsedFA val) = val
eval (ParsedPA val) = val

data AParsedLipid = forall a. AParsedLipid (ParsedLipid a)

--inputParser =  faParser
--           <|> paParser
showFA :: FA -> String
showFA = show
showPA :: PA -> String
showPA = show

showLipid :: AParsedLipid -> String
showLipid (AParsedLipid (ParsedFA x)) = "AParsedLipid (ParsedFA "++ showFA x ++")"
showLipid (AParsedLipid (ParsedPA x)) = "AParsedLipid (ParsedPA "++ showPA x ++")"

parser :: Parser AParsedLipid
parser = ((AParsedLipid . ParsedFA) <$> faParser) <|> ((AParsedLipid . ParsedPA) <$> paParser)

use str = evaluate $ parseOnly paParser str
