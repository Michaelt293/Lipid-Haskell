{-|
Module      : Lipid.Parsers.CombinedRadyl.GlycerophospholipidSpec
Description :
Copyright   : Michael Thomas
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental
-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Lipid.Parsers.CombinedRadyl.GlycerophospholipidSpec where

import Lipid.Blocks
import Test.Hspec
import Lipid.CombinedRadyl.Glycerophospholipid
import Lipid.Parsers.CombinedRadyl.Glycerophospholipid


spec :: Spec
spec = do
  describe "Test for quasiquoters and Shorthand instances" $ do
    it "QuasiQuoter for PA 34:0" $
      shorthand @ (PA (Maybe DeltaPosition)) [paMaybeDelta|PA 34:0|] `shouldBe` "PA 34:0"
    it "QuasiQuoter for PA 34:0" $
      shorthand @ (PA DeltaPosition) [paDelta|PA 34:0|] `shouldBe` "PA 34:0"
    it "QuasiQuoter for PA 34:1(9)(6)" $
      shorthand [paMaybeDelta|PA 34:1(9)(6)|] `shouldBe` "PA 34:1(9)(6)"
    it "QuasiQuoter for PA 34:1(9)(6)" $
      shorthand [paDelta|PA 34:1(9)(6)|] `shouldBe` "PA 34:1(9)(6)"
    it "QuasiQuoter for PA 34:1(9Z)(6Z)" $
      shorthand [paMaybeDelta|PA 34:1(9Z)(6Z)|] `shouldBe` "PA 34:1(9Z)(6Z)"
    it "QuasiQuoter for PA 34:1(9Z)(6Z)" $
      shorthand [paDelta|PA 34:1(9Z)(6Z)|] `shouldBe` "PA 34:1(9Z)(6Z)"
    it "QuasiQuoter for PA 34:1" $
      shorthand @ (PA (Maybe DeltaPosition)) [paMaybeDelta|PA 34:1|] `shouldBe` "PA 34:1"

    it "QuasiQuoter for PE 34:0" $
      shorthand @ (PE (Maybe DeltaPosition)) [peMaybeDelta|PE 34:0|] `shouldBe` "PE 34:0"
    it "QuasiQuoter for PE 34:0" $
      shorthand @ (PE DeltaPosition) [peDelta|PE 34:0|] `shouldBe` "PE 34:0"
    it "QuasiQuoter for PE 34:1(9)(6)" $
      shorthand [peMaybeDelta|PE 34:1(9)(6)|] `shouldBe` "PE 34:1(9)(6)"
    it "QuasiQuoter for PE 34:1(9)(6)" $
      shorthand [peDelta|PE 34:1(9)(6)|] `shouldBe` "PE 34:1(9)(6)"
    it "QuasiQuoter for PE 34:1(9Z)(6Z)" $
      shorthand [peMaybeDelta|PE 34:1(9Z)(6Z)|] `shouldBe` "PE 34:1(9Z)(6Z)"
    it "QuasiQuoter for PE 34:1(9Z)(6Z)" $
      shorthand [peDelta|PE 34:1(9Z)(6Z)|] `shouldBe` "PE 34:1(9Z)(6Z)"
    it "QuasiQuoter for PE 34:1" $
      shorthand @ (PE (Maybe DeltaPosition)) [peMaybeDelta|PE 34:1|] `shouldBe` "PE 34:1"

    it "QuasiQuoter for PG 34:1(9Z)(6Z)" $
      shorthand [pgMaybeDelta|PG 34:1(9Z)(6Z)|] `shouldBe` "PG 34:1(9Z)(6Z)"
    it "QuasiQuoter for PG 34:1(9Z)(6Z)" $
      shorthand [pgDelta|PG 34:1(9Z)(6Z)|] `shouldBe` "PG 34:1(9Z)(6Z)"
    it "QuasiQuoter for PG 34:1" $
      shorthand @ (PG (Maybe DeltaPosition)) [pgMaybeDelta|PG 34:1|] `shouldBe` "PG 34:1"

    it "QuasiQuoter for PS 34:1(9Z)(6Z)" $
      shorthand [psMaybeDelta|PS 34:1(9Z)(6Z)|] `shouldBe` "PS 34:1(9Z)(6Z)"
    it "QuasiQuoter for PS 34:1(9Z)(6Z)" $
      shorthand [psDelta|PS 34:1(9Z)(6Z)|] `shouldBe` "PS 34:1(9Z)(6Z)"
    it "QuasiQuoter for PS 34:1" $
      shorthand @ (PS (Maybe DeltaPosition)) [psMaybeDelta|PS 34:1|] `shouldBe` "PS 34:1"

    it "QuasiQuoter for PI 34:1(9Z)(6Z)" $
      shorthand [piMaybeDelta|PI 34:1(9Z)(6Z)|] `shouldBe` "PI 34:1(9Z)(6Z)"
    it "QuasiQuoter for PI 34:1(9Z)(6Z)" $
      shorthand [piDelta|PI 34:1(9Z)(6Z)|] `shouldBe` "PI 34:1(9Z)(6Z)"
    it "QuasiQuoter for PI 34:1" $
      shorthand @ (PI (Maybe DeltaPosition)) [piMaybeDelta|PI 34:1|] `shouldBe` "PI 34:1"

    it "QuasiQuoter for PGP 34:1(9Z)(6Z)" $
      shorthand [pgpMaybeDelta|PGP 34:1(9Z)(6Z)|] `shouldBe` "PGP 34:1(9Z)(6Z)"
    it "QuasiQuoter for PGP 34:1(9Z)(6Z)" $
      shorthand [pgpDelta|PGP 34:1(9Z)(6Z)|] `shouldBe` "PGP 34:1(9Z)(6Z)"
    it "QuasiQuoter for PGP 34:1" $
      shorthand @ (PGP (Maybe DeltaPosition)) [pgpMaybeDelta|PGP 34:1|] `shouldBe` "PGP 34:1"

    it "QuasiQuoter for PC 34:1(9Z)(6Z)" $
      shorthand [pcMaybeDelta|PC 34:1(9Z)(6Z)|] `shouldBe` "PC 34:1(9Z)(6Z)"
    it "QuasiQuoter for PC 34:1(9Z)(6Z)" $
      shorthand [pcDelta|PC 34:1(9Z)(6Z)|] `shouldBe` "PC 34:1(9Z)(6Z)"
    it "QuasiQuoter for PC 34:1" $
      shorthand @ (PC (Maybe DeltaPosition)) [pcMaybeDelta|PC 34:1|] `shouldBe` "PC 34:1"

    it "QuasiQuoter for PIP 34:1(9Z)(6Z)" $
      shorthand [pipMaybeDelta|PIP 34:1(9Z)(6Z)|] `shouldBe` "PIP 34:1(9Z)(6Z)"
    it "QuasiQuoter for PIP 34:1(9Z)(6Z)" $
      shorthand [pipDelta|PIP 34:1(9Z)(6Z)|] `shouldBe` "PIP 34:1(9Z)(6Z)"
    it "QuasiQuoter for PIP 34:1" $
      shorthand @ (PIP (Maybe DeltaPosition)) [pipMaybeDelta|PIP 34:1|] `shouldBe` "PIP 34:1"

    it "QuasiQuoter for PIP2 34:1(9Z)(6Z)" $
      shorthand [pip2MaybeDelta|PIP2 34:1(9Z)(6Z)|] `shouldBe` "PIP2 34:1(9Z)(6Z)"
    it "QuasiQuoter for PIP2 34:1(9Z)(6Z)" $
      shorthand [pip2Delta|PIP2 34:1(9Z)(6Z)|] `shouldBe` "PIP2 34:1(9Z)(6Z)"
    it "QuasiQuoter for PIP2 34:1" $
      shorthand @ (PIP2 (Maybe DeltaPosition)) [pip2MaybeDelta|PIP2 34:1|] `shouldBe` "PIP2 34:1"

  describe "Test for quasiquoters and nNomenclature instances" $ do
    it "QuasiQuoter for PA 34:0" $
      nNomenclature @ (PA (Maybe OmegaPosition)) [paMaybeOmega|PA 34:0|] `shouldBe` "PA 34:0"
    it "QuasiQuoter for PA 34:0" $
      nNomenclature @ (PA OmegaPosition) [paDelta|PA 34:0|] `shouldBe` "PA 34:0"
    it "QuasiQuoter for PA 34:1(n-9)(n-6)" $
      nNomenclature [paMaybeOmega|PA 34:1(n-9)(n-6)|] `shouldBe` "PA 34:1(n-9)(n-6)"
    it "QuasiQuoter for PA 34:1(n-9)(n-6)" $
      nNomenclature [paOmega|PA 34:1(n-9)(n-6)|] `shouldBe` "PA 34:1(n-9)(n-6)"
    it "QuasiQuoter for PA 34:1(n-9Z)(n-6Z)" $
      nNomenclature [paMaybeOmega|PA 34:1(n-9)(n-6)|] `shouldBe` "PA 34:1(n-9)(n-6)"
    it "QuasiQuoter for PA 34:1(n-9)(n-6)" $
      nNomenclature [paOmega|PA 34:1(n-9)(n-6)|] `shouldBe` "PA 34:1(n-9)(n-6)"
    it "QuasiQuoter for PA 34:1" $
      nNomenclature @ (PA (Maybe OmegaPosition)) [paMaybeOmega|PA 34:1|] `shouldBe` "PA 34:1"

    it "QuasiQuoter for PE 34:1(n-9Z)(n-6Z)" $
      nNomenclature [peMaybeOmega|PE 34:1(n-9)(n-6)|] `shouldBe` "PE 34:1(n-9)(n-6)"
    it "QuasiQuoter for PE 34:1(n-9)(n-6)" $
      nNomenclature [peOmega|PE 34:1(n-9)(n-6)|] `shouldBe` "PE 34:1(n-9)(n-6)"
    it "QuasiQuoter for PE 34:1" $
      nNomenclature @ (PE (Maybe OmegaPosition)) [peMaybeOmega|PE 34:1|] `shouldBe` "PE 34:1"

    it "QuasiQuoter for PC 34:1(n-9Z)(n-6Z)" $
      nNomenclature [pcMaybeOmega|PC 34:1(n-9)(n-6)|] `shouldBe` "PC 34:1(n-9)(n-6)"
    it "QuasiQuoter for PC 34:1(n-9)(n-6)" $
      nNomenclature [pcOmega|PC 34:1(n-9)(n-6)|] `shouldBe` "PC 34:1(n-9)(n-6)"
    it "QuasiQuoter for PC 34:1" $
      nNomenclature @ (PC (Maybe OmegaPosition)) [pcMaybeOmega|PC 34:1|] `shouldBe` "PC 34:1"

    it "QuasiQuoter for PS 34:1(n-9Z)(n-6Z)" $
      nNomenclature [psMaybeOmega|PS 34:1(n-9)(n-6)|] `shouldBe` "PS 34:1(n-9)(n-6)"
    it "QuasiQuoter for PS 34:1(n-9)(n-6)" $
      nNomenclature [psOmega|PS 34:1(n-9)(n-6)|] `shouldBe` "PS 34:1(n-9)(n-6)"
    it "QuasiQuoter for PS 34:1" $
      nNomenclature @ (PS (Maybe OmegaPosition)) [psMaybeOmega|PS 34:1|] `shouldBe` "PS 34:1"

    it "QuasiQuoter for PG 34:1(n-9Z)(n-6Z)" $
      nNomenclature [pgMaybeOmega|PG 34:1(n-9)(n-6)|] `shouldBe` "PG 34:1(n-9)(n-6)"
    it "QuasiQuoter for PG 34:1(n-9)(n-6)" $
      nNomenclature [pgOmega|PG 34:1(n-9)(n-6)|] `shouldBe` "PG 34:1(n-9)(n-6)"
    it "QuasiQuoter for PG 34:1" $
      nNomenclature @ (PG (Maybe OmegaPosition)) [pgMaybeOmega|PG 34:1|] `shouldBe` "PG 34:1"

    it "QuasiQuoter for PGP 34:1(n-9Z)(n-6Z)" $
      nNomenclature [pgpMaybeOmega|PGP 34:1(n-9)(n-6)|] `shouldBe` "PGP 34:1(n-9)(n-6)"
    it "QuasiQuoter for PGP 34:1(n-9)(n-6)" $
      nNomenclature [pgpOmega|PGP 34:1(n-9)(n-6)|] `shouldBe` "PGP 34:1(n-9)(n-6)"
    it "QuasiQuoter for PGP 34:1" $
      nNomenclature @ (PGP (Maybe OmegaPosition)) [pgpMaybeOmega|PGP 34:1|] `shouldBe` "PGP 34:1"

    it "QuasiQuoter for PI 34:1(n-9Z)(n-6Z)" $
      nNomenclature [piMaybeOmega|PI 34:1(n-9)(n-6)|] `shouldBe` "PI 34:1(n-9)(n-6)"
    it "QuasiQuoter for PI 34:1(n-9)(n-6)" $
      nNomenclature [piOmega|PI 34:1(n-9)(n-6)|] `shouldBe` "PI 34:1(n-9)(n-6)"
    it "QuasiQuoter for PI 34:1" $
      nNomenclature @ (PI (Maybe OmegaPosition)) [piMaybeOmega|PI 34:1|] `shouldBe` "PI 34:1"

    it "QuasiQuoter for PIP 34:1(n-9Z)(n-6Z)" $
      nNomenclature [pipMaybeOmega|PIP 34:1(n-9)(n-6)|] `shouldBe` "PIP 34:1(n-9)(n-6)"
    it "QuasiQuoter for PIP 34:1(n-9)(n-6)" $
      nNomenclature [pipOmega|PIP 34:1(n-9)(n-6)|] `shouldBe` "PIP 34:1(n-9)(n-6)"
    it "QuasiQuoter for PIP 34:1" $
      nNomenclature @ (PIP (Maybe OmegaPosition)) [pipMaybeOmega|PIP 34:1|] `shouldBe` "PIP 34:1"

    it "QuasiQuoter for PIP2 34:1(n-9Z)(n-6Z)" $
      nNomenclature [pip2MaybeOmega|PIP2 34:1(n-9)(n-6)|] `shouldBe` "PIP2 34:1(n-9)(n-6)"
    it "QuasiQuoter for PIP2 34:1(n-9)(n-6)" $
      nNomenclature [pip2Omega|PIP2 34:1(n-9)(n-6)|] `shouldBe` "PIP2 34:1(n-9)(n-6)"
    it "QuasiQuoter for PIP2 34:1" $
      nNomenclature @ (PIP2 (Maybe OmegaPosition)) [pip2MaybeOmega|PIP2 34:1|] `shouldBe` "PIP2 34:1"
