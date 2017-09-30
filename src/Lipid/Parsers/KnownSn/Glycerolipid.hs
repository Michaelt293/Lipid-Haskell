{-|
Module      : Lipid.Parsers.KnownSn.Glycerolipid
Description :
Copyright   : Michael Thomas
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Lipid.Parsers.KnownSn.Glycerolipid where

import Lipid.Parsers.Blocks
import Lipid.KnownSn.Glycerolipid
import Lipid.Blocks
import Data.Monoid ((<>))
import Text.Megaparsec
import Text.Megaparsec.String
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lift

glyceroLipidP :: Parser a -> Parser b -> Parser c -> Parser (a, b, c)
glyceroLipidP p1 p2 p3 = do
  pos1 <- p1
  _ <- char '/'
  pos2 <- p2
  _ <- char '/'
  pos3 <- p3
  return (pos1, pos2, pos3)

tgP :: Parser (Radyl a) -> Parser (TG a)
tgP p = do
  _ <- string "TG "
  (r1, r2, r3) <- glyceroLipidP p p p
  return . TG $ Glycerol r1 r2 r3

tgDeltaP :: Parser (TG DeltaPosition)
tgDeltaP = tgP radylDeltaP

tgMaybeDeltaP :: Parser (TG (Maybe DeltaPosition))
tgMaybeDeltaP = tgP radylMaybeDeltaP

tgOmegaP :: Parser (TG OmegaPosition)
tgOmegaP = tgP radylOmegaP

tgMaybeOmegaP :: Parser (TG (Maybe OmegaPosition))
tgMaybeOmegaP = tgP radylMaybeOmegaP

quoteTgDelta :: String -> Q Exp
quoteTgDelta s =
  case parse (tgDeltaP <* eof) "" s of
    Left err -> fail $
      "Could not parse lipid!\n" <> parseErrorPretty err
    Right v  -> lift v

tgDelta :: QuasiQuoter
tgDelta = QuasiQuoter
  { quoteExp = quoteTgDelta
  , quotePat  = notHandled "patterns" "tgDelta"
  , quoteType = notHandled "types" "tgDelta"
  , quoteDec  = notHandled "declarations" "tgDelta"
  }

quoteTgMaybeDelta :: String -> Q Exp
quoteTgMaybeDelta s =
  case parse (tgMaybeDeltaP <* eof) "" s of
    Left err -> fail $
      "Could not parse lipid!\n" <> parseErrorPretty err
    Right v  -> lift v

tgMaybeDelta :: QuasiQuoter
tgMaybeDelta = QuasiQuoter
  { quoteExp  = quoteTgMaybeDelta
  , quotePat  = notHandled "patterns" "tgMaybeDelta"
  , quoteType = notHandled "types" "tgMaybeDelta"
  , quoteDec  = notHandled "declarations" "tgMaybeDelta"
  }

quoteTgOmega :: String -> Q Exp
quoteTgOmega s =
  case parse (tgOmegaP <* eof) "" s of
    Left err -> fail $
      "Could not parse lipid!\n" <> parseErrorPretty err
    Right v  -> lift v

tgOmega :: QuasiQuoter
tgOmega = QuasiQuoter
  { quoteExp  = quoteTgOmega
  , quotePat  = notHandled "patterns" "tgOmega"
  , quoteType = notHandled "types" "tgOmega"
  , quoteDec  = notHandled "declarations" "tgOmega"
  }

quoteTgMaybeOmega :: String -> Q Exp
quoteTgMaybeOmega s =
  case parse (tgMaybeOmegaP <* eof) "" s of
    Left err -> fail $
      "Could not parse lipid!\n" <> parseErrorPretty err
    Right v  -> lift v

tgMaybeOmega :: QuasiQuoter
tgMaybeOmega = QuasiQuoter
  { quoteExp  = quoteTgMaybeOmega
  , quotePat  = notHandled "patterns" "tgMaybeOmega"
  , quoteType = notHandled "types" "tgMaybeOmega"
  , quoteDec  = notHandled "declarations" "tgMaybeOmega"
  }

dg12P :: Parser (Radyl a) -> Parser (DG a)
dg12P p = do
  (r1, r2, _) <- glyceroLipidP p p glycerolHydroxylP
  return . DG12 $ Glycerol r1 r2 GlycerolHydroxyl

dg13P :: Parser (Radyl a) -> Parser (DG a)
dg13P p = do
  (r1, _, r3) <- glyceroLipidP p glycerolHydroxylP p
  return . DG13 $ Glycerol r1 GlycerolHydroxyl r3

dg23P :: Parser (Radyl a) -> Parser (DG a)
dg23P p = do
  (_, r2, r3) <- glyceroLipidP glycerolHydroxylP p p
  return . DG23 $ Glycerol GlycerolHydroxyl r2 r3

dgP :: Parser (Radyl a) -> Parser (DG a)
dgP p = string "DG " *> dg12P p <|> dg13P p <|> dg23P p

dgDeltaP :: Parser (DG DeltaPosition)
dgDeltaP = dgP radylDeltaP

dgMaybeDeltaP :: Parser (DG (Maybe DeltaPosition))
dgMaybeDeltaP = dgP radylMaybeDeltaP

dgOmegaP :: Parser (DG OmegaPosition)
dgOmegaP = dgP radylOmegaP

dgMaybeOmegaP :: Parser (DG (Maybe OmegaPosition))
dgMaybeOmegaP = dgP radylMaybeOmegaP

quoteDgDelta :: String -> Q Exp
quoteDgDelta s =
  case parse (tgDeltaP <* eof) "" s of
    Left err -> fail $
      "Could not parse elemental formula!\n" <> parseErrorPretty err
    Right v  -> lift v

dgDelta :: QuasiQuoter
dgDelta = QuasiQuoter
  { quoteExp = quoteDgDelta
  , quotePat  = notHandled "patterns" "dgDelta"
  , quoteType = notHandled "types" "dgDelta"
  , quoteDec  = notHandled "declarations" "dgDelta"
  }

quoteDgMaybeDelta :: String -> Q Exp
quoteDgMaybeDelta s =
  case parse (dgMaybeDeltaP <* eof) "" s of
    Left err -> fail $
      "Could not parse lipid!\n" <> parseErrorPretty err
    Right v  -> lift v

dgMaybeDelta :: QuasiQuoter
dgMaybeDelta = QuasiQuoter
  { quoteExp  = quoteDgMaybeDelta
  , quotePat  = notHandled "patterns" "dgMaybeDelta"
  , quoteType = notHandled "types" "dgMaybeDelta"
  , quoteDec  = notHandled "declarations" "dgMaybeDelta"
  }

quoteDgOmega :: String -> Q Exp
quoteDgOmega s =
  case parse (dgOmegaP <* eof) "" s of
    Left err -> fail $
      "Could not parse lipid!\n" <> parseErrorPretty err
    Right v  -> lift v

dgOmega :: QuasiQuoter
dgOmega = QuasiQuoter
  { quoteExp  = quoteDgOmega
  , quotePat  = notHandled "patterns" "dgOmega"
  , quoteType = notHandled "types" "dgOmega"
  , quoteDec  = notHandled "declarations" "dgOmega"
  }

quoteDgMaybeOmega :: String -> Q Exp
quoteDgMaybeOmega s =
  case parse (dgMaybeOmegaP <* eof) "" s of
    Left err -> fail $
      "Could not parse lipid!\n" <> parseErrorPretty err
    Right v  -> lift v

dgMaybeOmega :: QuasiQuoter
dgMaybeOmega = QuasiQuoter
  { quoteExp  = quoteDgMaybeOmega
  , quotePat  = notHandled "patterns" "dgMaybeOmega"
  , quoteType = notHandled "types" "dgMaybeOmega"
  , quoteDec  = notHandled "declarations" "dgMaybeOmega"
  }

mg1P :: Parser (Radyl a) -> Parser (MG a)
mg1P p = do
  (r, _, _) <- glyceroLipidP p glycerolHydroxylP glycerolHydroxylP
  return . MG1 $ Glycerol r GlycerolHydroxyl GlycerolHydroxyl

mg2P :: Parser (Radyl a) -> Parser (MG a)
mg2P p = do
  (_, r, _) <- glyceroLipidP glycerolHydroxylP p glycerolHydroxylP
  return . MG2 $ Glycerol GlycerolHydroxyl r GlycerolHydroxyl

mg3P :: Parser (Radyl a) -> Parser (MG a)
mg3P p = do
  (_, _, r) <- glyceroLipidP glycerolHydroxylP glycerolHydroxylP p
  return . MG3 $ Glycerol GlycerolHydroxyl GlycerolHydroxyl r

mgP :: Parser (Radyl a) -> Parser (MG a)
mgP p = string "MG " *> mg1P p <|> mg2P p <|> mg3P p

mgDeltaP :: Parser (MG DeltaPosition)
mgDeltaP = mgP radylDeltaP

mgMaybeDeltaP :: Parser (MG (Maybe DeltaPosition))
mgMaybeDeltaP = mgP radylMaybeDeltaP

mgOmegaP :: Parser (MG OmegaPosition)
mgOmegaP = mgP radylOmegaP

mgMaybeOmegaP :: Parser (MG (Maybe OmegaPosition))
mgMaybeOmegaP = mgP radylMaybeOmegaP

quoteMgDelta :: String -> Q Exp
quoteMgDelta s =
  case parse (tgDeltaP <* eof) "" s of
    Left err -> fail $
      "Could not parse elemental formula!\n" <> parseErrorPretty err
    Right v  -> lift v

mgDelta :: QuasiQuoter
mgDelta = QuasiQuoter
  { quoteExp = quoteMgDelta
  , quotePat  = notHandled "patterns"
  , quoteType = notHandled "types"
  , quoteDec  = notHandled "declarations"
  }

quoteMgMaybeDelta :: String -> Q Exp
quoteMgMaybeDelta s =
  case parse (mgMaybeDeltaP <* eof) "" s of
    Left err -> fail $
      "Could not parse lipid!\n" <> parseErrorPretty err
    Right v  -> lift v

mgMaybeDelta :: QuasiQuoter
mgMaybeDelta = QuasiQuoter
  { quoteExp  = quoteMgMaybeDelta
  , quotePat  = notHandled "patterns"
  , quoteType = notHandled "types"
  , quoteDec  = notHandled "declarations"
  }

quoteMgOmega :: String -> Q Exp
quoteMgOmega s =
  case parse (mgOmegaP <* eof) "" s of
    Left err -> fail $
      "Could not parse lipid!\n" <> parseErrorPretty err
    Right v  -> lift v

mgOmega :: QuasiQuoter
mgOmega = QuasiQuoter
  { quoteExp  = quoteMgOmega
  , quotePat  = notHandled "patterns"
  , quoteType = notHandled "types"
  , quoteDec  = notHandled "declarations"
  }

quoteMgMaybeOmega :: String -> Q Exp
quoteMgMaybeOmega s =
  case parse (mgMaybeOmegaP <* eof) "" s of
    Left err -> fail $
      "Could not parse lipid!\n" <> parseErrorPretty err
    Right v  -> lift v

mgMaybeOmega :: QuasiQuoter
mgMaybeOmega = QuasiQuoter
  { quoteExp  = quoteMgMaybeOmega
  , quotePat  = notHandled "patterns"
  , quoteType = notHandled "types"
  , quoteDec  = notHandled "declarations"
  }

$(deriveLift ''TG)

$(deriveLift ''DG)

$(deriveLift ''MG)
