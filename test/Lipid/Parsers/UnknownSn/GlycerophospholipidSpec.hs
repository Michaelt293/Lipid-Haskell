{-|
Module      : Lipid.Parsers.UnknownSn.GlycerophospholipidSpec
Description :
Copyright   : Michael Thomas
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental
-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Lipid.Parsers.UnknownSn.GlycerophospholipidSpec where

import Lipid.Blocks
import Test.Hspec
import Lipid.UnknownSn.Glycerophospholipid
import Lipid.Parsers.UnknownSn.Glycerophospholipid


spec :: Spec
spec = do
  describe "Test for quasiquoters and Shorthand instances" $ do
    it "QuasiQuoter for PA 14:0_16:0" $
      shorthand @ (PA (Maybe DeltaPosition)) [paMaybeDelta|PA 14:0_16:0|] `shouldBe` "PA 14:0_16:0"
    it "QuasiQuoter for PA 14:0_16:0" $
      shorthand @ (PA DeltaPosition) [paDelta|PA 14:0_16:0|] `shouldBe` "PA 14:0_16:0"
    it "QuasiQuoter for PA 14:0_18:1(9)" $
      shorthand [paMaybeDelta|PA 14:0_18:1(9)|] `shouldBe` "PA 14:0_18:1(9)"
    it "QuasiQuoter for PA 14:0_18:1(9)" $
      shorthand [paDelta|PA 14:0_18:1(9)|] `shouldBe` "PA 14:0_18:1(9)"
    it "QuasiQuoter for PA 14:0_18:1(9Z)" $
      shorthand [paMaybeDelta|PA 14:0_18:1(9Z)|] `shouldBe` "PA 14:0_18:1(9Z)"
    it "QuasiQuoter for PA 14:0_18:1(9Z)" $
      shorthand [paDelta|PA 14:0_18:1(9Z)|] `shouldBe` "PA 14:0_18:1(9Z)"
    it "QuasiQuoter for PA 14:0_18:1" $
      shorthand @ (PA (Maybe DeltaPosition)) [paMaybeDelta|PA 14:0_18:1|] `shouldBe` "PA 14:0_18:1"

    it "QuasiQuoter for PE 14:0_16:0" $
      shorthand @ (PE (Maybe DeltaPosition)) [peMaybeDelta|PE 14:0_16:0|] `shouldBe` "PE 14:0_16:0"
    it "QuasiQuoter for PE 14:0_16:0" $
      shorthand @ (PE DeltaPosition) [peDelta|PE 14:0_16:0|] `shouldBe` "PE 14:0_16:0"
    it "QuasiQuoter for PE 14:0_18:1(9)" $
      shorthand [peMaybeDelta|PE 14:0_18:1(9)|] `shouldBe` "PE 14:0_18:1(9)"
    it "QuasiQuoter for PE 14:0_18:1(9)" $
      shorthand [peDelta|PE 14:0_18:1(9)|] `shouldBe` "PE 14:0_18:1(9)"
    it "QuasiQuoter for PE 14:0_18:1(9Z)" $
      shorthand [peMaybeDelta|PE 14:0_18:1(9Z)|] `shouldBe` "PE 14:0_18:1(9Z)"
    it "QuasiQuoter for PE 14:0_18:1(9Z)" $
      shorthand [peDelta|PE 14:0_18:1(9Z)|] `shouldBe` "PE 14:0_18:1(9Z)"
    it "QuasiQuoter for PE 14:0_18:1" $
      shorthand @ (PE (Maybe DeltaPosition)) [peMaybeDelta|PE 14:0_18:1|] `shouldBe` "PE 14:0_18:1"

    it "QuasiQuoter for PG 14:0_18:1(9Z)" $
      shorthand [pgMaybeDelta|PG 14:0_18:1(9Z)|] `shouldBe` "PG 14:0_18:1(9Z)"
    it "QuasiQuoter for PG 14:0_18:1(9Z)" $
      shorthand [pgDelta|PG 14:0_18:1(9Z)|] `shouldBe` "PG 14:0_18:1(9Z)"
    it "QuasiQuoter for PG 14:0_18:1" $
      shorthand @ (PG (Maybe DeltaPosition)) [pgMaybeDelta|PG 14:0_18:1|] `shouldBe` "PG 14:0_18:1"

    it "QuasiQuoter for PS 14:0_18:1(9Z)" $
      shorthand [psMaybeDelta|PS 14:0_18:1(9Z)|] `shouldBe` "PS 14:0_18:1(9Z)"
    it "QuasiQuoter for PS 14:0_18:1(9Z)" $
      shorthand [psDelta|PS 14:0_18:1(9Z)|] `shouldBe` "PS 14:0_18:1(9Z)"
    it "QuasiQuoter for PS 14:0_18:1" $
      shorthand @ (PS (Maybe DeltaPosition)) [psMaybeDelta|PS 14:0_18:1|] `shouldBe` "PS 14:0_18:1"

    it "QuasiQuoter for PI 14:0_18:1(9Z)" $
      shorthand [piMaybeDelta|PI 14:0_18:1(9Z)|] `shouldBe` "PI 14:0_18:1(9Z)"
    it "QuasiQuoter for PI 14:0_18:1(9Z)" $
      shorthand [piDelta|PI 14:0_18:1(9Z)|] `shouldBe` "PI 14:0_18:1(9Z)"
    it "QuasiQuoter for PI 14:0_18:1" $
      shorthand @ (PI (Maybe DeltaPosition)) [piMaybeDelta|PI 14:0_18:1|] `shouldBe` "PI 14:0_18:1"

    it "QuasiQuoter for PGP 14:0_18:1(9Z)" $
      shorthand [pgpMaybeDelta|PGP 14:0_18:1(9Z)|] `shouldBe` "PGP 14:0_18:1(9Z)"
    it "QuasiQuoter for PGP 14:0_18:1(9Z)" $
      shorthand [pgpDelta|PGP 14:0_18:1(9Z)|] `shouldBe` "PGP 14:0_18:1(9Z)"
    it "QuasiQuoter for PGP 14:0_18:1" $
      shorthand @ (PGP (Maybe DeltaPosition)) [pgpMaybeDelta|PGP 14:0_18:1|] `shouldBe` "PGP 14:0_18:1"

    it "QuasiQuoter for PC 14:0_18:1(9Z)" $
      shorthand [pcMaybeDelta|PC 14:0_18:1(9Z)|] `shouldBe` "PC 14:0_18:1(9Z)"
    it "QuasiQuoter for PC 14:0_18:1(9Z)" $
      shorthand [pcDelta|PC 14:0_18:1(9Z)|] `shouldBe` "PC 14:0_18:1(9Z)"
    it "QuasiQuoter for PC 14:0_18:1" $
      shorthand @ (PC (Maybe DeltaPosition)) [pcMaybeDelta|PC 14:0_18:1|] `shouldBe` "PC 14:0_18:1"

    it "QuasiQuoter for PIP 14:0_18:1(9Z)" $
      shorthand [pipMaybeDelta|PIP 14:0_18:1(9Z)|] `shouldBe` "PIP 14:0_18:1(9Z)"
    it "QuasiQuoter for PIP 14:0_18:1(9Z)" $
      shorthand [pipDelta|PIP 14:0_18:1(9Z)|] `shouldBe` "PIP 14:0_18:1(9Z)"
    it "QuasiQuoter for PIP 14:0_18:1" $
      shorthand @ (PIP (Maybe DeltaPosition)) [pipMaybeDelta|PIP 14:0_18:1|] `shouldBe` "PIP 14:0_18:1"

    it "QuasiQuoter for PIP2 14:0_18:1(9Z)" $
      shorthand [pip2MaybeDelta|PIP2 14:0_18:1(9Z)|] `shouldBe` "PIP2 14:0_18:1(9Z)"
    it "QuasiQuoter for PIP2 14:0_18:1(9Z)" $
      shorthand [pip2Delta|PIP2 14:0_18:1(9Z)|] `shouldBe` "PIP2 14:0_18:1(9Z)"
    it "QuasiQuoter for PIP2 14:0_18:1" $
      shorthand @ (PIP2 (Maybe DeltaPosition)) [pip2MaybeDelta|PIP2 14:0_18:1|] `shouldBe` "PIP2 14:0_18:1"

  describe "Test for quasiquoters and nNomenclature instances" $ do
    it "QuasiQuoter for PA 14:0_16:0" $
      nNomenclature @ (PA (Maybe OmegaPosition)) [paMaybeOmega|PA 14:0_16:0|] `shouldBe` "PA 14:0_16:0"
    it "QuasiQuoter for PA 14:0_16:0" $
      nNomenclature @ (PA OmegaPosition) [paDelta|PA 14:0_16:0|] `shouldBe` "PA 14:0_16:0"
    it "QuasiQuoter for PA 14:0_18:1(n-9)(n-6)" $
      nNomenclature [paMaybeOmega|PA 14:0_18:1(n-9)|] `shouldBe` "PA 14:0_18:1(n-9)"
    it "QuasiQuoter for PA 14:0_18:1(n-9)" $
      nNomenclature [paOmega|PA 14:0_18:1(n-9)|] `shouldBe` "PA 14:0_18:1(n-9)"
    it "QuasiQuoter for PA 14:0_18:1(n-9Z)" $
      nNomenclature [paMaybeOmega|PA 14:0_18:1(n-9Z)|] `shouldBe` "PA 14:0_18:1(n-9)"
    it "QuasiQuoter for PA 14:0_18:1(n-9)" $
      nNomenclature [paOmega|PA 14:0_18:1(n-9)|] `shouldBe` "PA 14:0_18:1(n-9)"
    it "QuasiQuoter for PA 14:0_18:1" $
      nNomenclature @ (PA (Maybe OmegaPosition)) [paMaybeOmega|PA 14:0_18:1|] `shouldBe` "PA 14:0_18:1"

    it "QuasiQuoter for PE 14:0_18:1(n-9Z)" $
      nNomenclature [peMaybeOmega|PE 14:0_18:1(n-9Z)|] `shouldBe` "PE 14:0_18:1(n-9)"
    it "QuasiQuoter for PE 14:0_18:1(n-9)(n-6)" $
      nNomenclature [peOmega|PE 14:0_18:1(n-9)|] `shouldBe` "PE 14:0_18:1(n-9)"
    it "QuasiQuoter for PE 14:0_18:1" $
      nNomenclature @ (PE (Maybe OmegaPosition)) [peMaybeOmega|PE 14:0_18:1|] `shouldBe` "PE 14:0_18:1"

    it "QuasiQuoter for PC 14:0_18:1(n-9Z)" $
      nNomenclature [pcMaybeOmega|PC 14:0_18:1(n-9Z)|] `shouldBe` "PC 14:0_18:1(n-9)"
    it "QuasiQuoter for PC 14:0_18:1(n-9)" $
      nNomenclature [pcOmega|PC 14:0_18:1(n-9)|] `shouldBe` "PC 14:0_18:1(n-9)"
    it "QuasiQuoter for PC 14:0_18:1" $
      nNomenclature @ (PC (Maybe OmegaPosition)) [pcMaybeOmega|PC 14:0_18:1|] `shouldBe` "PC 14:0_18:1"

    it "QuasiQuoter for PS 14:0_18:1(n-9Z)" $
      nNomenclature [psMaybeOmega|PS 14:0_18:1(n-9Z)|] `shouldBe` "PS 14:0_18:1(n-9)"
    it "QuasiQuoter for PS 14:0_18:1(n-9)" $
      nNomenclature [psOmega|PS 14:0_18:1(n-9)|] `shouldBe` "PS 14:0_18:1(n-9)"
    it "QuasiQuoter for PS 14:0_18:1" $
      nNomenclature @ (PS (Maybe OmegaPosition)) [psMaybeOmega|PS 14:0_18:1|] `shouldBe` "PS 14:0_18:1"

    it "QuasiQuoter for PG 14:0_18:1(n-9Z)" $
      nNomenclature [pgMaybeOmega|PG 14:0_18:1(n-9Z)|] `shouldBe` "PG 14:0_18:1(n-9)"
    it "QuasiQuoter for PG 14:0_18:1(n-9)" $
      nNomenclature [pgOmega|PG 14:0_18:1(n-9)|] `shouldBe` "PG 14:0_18:1(n-9)"
    it "QuasiQuoter for PG 14:0_18:1" $
      nNomenclature @ (PG (Maybe OmegaPosition)) [pgMaybeOmega|PG 14:0_18:1|] `shouldBe` "PG 14:0_18:1"

    it "QuasiQuoter for PGP 14:0_18:1(n-9Z)" $
      nNomenclature [pgpMaybeOmega|PGP 14:0_18:1(n-9Z)|] `shouldBe` "PGP 14:0_18:1(n-9)"
    it "QuasiQuoter for PGP 14:0_18:1(n-9)" $
      nNomenclature [pgpOmega|PGP 14:0_18:1(n-9)|] `shouldBe` "PGP 14:0_18:1(n-9)"
    it "QuasiQuoter for PGP 14:0_18:1" $
      nNomenclature @ (PGP (Maybe OmegaPosition)) [pgpMaybeOmega|PGP 14:0_18:1|] `shouldBe` "PGP 14:0_18:1"

    it "QuasiQuoter for PI 14:0_18:1(n-9Z)" $
      nNomenclature [piMaybeOmega|PI 14:0_18:1(n-9Z)|] `shouldBe` "PI 14:0_18:1(n-9)"
    it "QuasiQuoter for PI 14:0_18:1(n-9)(n-6)" $
      nNomenclature [piOmega|PI 14:0_18:1(n-9)|] `shouldBe` "PI 14:0_18:1(n-9)"
    it "QuasiQuoter for PI 14:0_18:1" $
      nNomenclature @ (PI (Maybe OmegaPosition)) [piMaybeOmega|PI 14:0_18:1|] `shouldBe` "PI 14:0_18:1"

    it "QuasiQuoter for PIP 14:0_18:1(n-9Z)" $
      nNomenclature [pipMaybeOmega|PIP 14:0_18:1(n-9Z)|] `shouldBe` "PIP 14:0_18:1(n-9)"
    it "QuasiQuoter for PIP 14:0_18:1(n-9)" $
      nNomenclature [pipOmega|PIP 14:0_18:1(n-9)|] `shouldBe` "PIP 14:0_18:1(n-9)"
    it "QuasiQuoter for PIP 14:0_18:1" $
      nNomenclature @ (PIP (Maybe OmegaPosition)) [pipMaybeOmega|PIP 14:0_18:1|] `shouldBe` "PIP 14:0_18:1"

    it "QuasiQuoter for PIP2 14:0_18:1(n-9Z)" $
      nNomenclature [pip2MaybeOmega|PIP2 14:0_18:1(n-9Z)|] `shouldBe` "PIP2 14:0_18:1(n-9)"
    it "QuasiQuoter for PIP2 14:0_18:1(n-9)" $
      nNomenclature [pip2Omega|PIP2 14:0_18:1(n-9)|] `shouldBe` "PIP2 14:0_18:1(n-9)"
    it "QuasiQuoter for PIP2 14:0_18:1" $
      nNomenclature @ (PIP2 (Maybe OmegaPosition)) [pip2MaybeOmega|PIP2 14:0_18:1|] `shouldBe` "PIP2 14:0_18:1"
