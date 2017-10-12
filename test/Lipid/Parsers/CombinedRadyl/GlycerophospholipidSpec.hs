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
