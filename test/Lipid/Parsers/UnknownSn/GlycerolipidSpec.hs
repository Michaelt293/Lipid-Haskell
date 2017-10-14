{-|
Module      : Lipid.Parsers.UnknownSn.GlycerolipidSpec
Description :
Copyright   : Michael Thomas
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental
-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Lipid.Parsers.UnknownSn.GlycerolipidSpec where

import Lipid.Blocks
import Test.Hspec
import Lipid.UnknownSn.Glycerolipid
import Lipid.Parsers.UnknownSn.Glycerolipid

spec :: Spec
spec = do
  describe "Test for quasiquoters and Shorthand instances" $ do
    it "QuasiQuoter for TG 16:0_18:1_22:6" $
      shorthand @ (TG (Maybe DeltaPosition)) [tgMaybeDelta|TG 16:0_18:1_22:6|] `shouldBe` "TG 16:0_18:1_22:6"
    it "QuasiQuoter for DG 16:0_22:6" $
      shorthand @ (DG (Maybe DeltaPosition)) [dgMaybeDelta|DG 16:0_22:6|] `shouldBe` "DG 16:0_22:6"
    it "QuasiQuoter for TG 16:0_18:1(9Z)_22:6" $
      shorthand [tgMaybeDelta|TG 16:0_18:1(9Z)_22:6|] `shouldBe` "TG 16:0_18:1(9Z)_22:6"
    it "QuasiQuoter for TG 16:0_18:1(9Z)_22:6(4,7,10,?,16,19)" $
      shorthand [tgMaybeDelta|TG 16:0_18:1(9Z)_22:6(4,7,10,?,16,19)|] `shouldBe` "TG 16:0_18:1(9Z)_22:6(4,7,10,?,16,19)"
    it "QuasiQuoter for TG 16:0_18:1(9Z)_22:6(4Z,7Z,10Z,13Z,16Z,19Z)" $
      shorthand [tgDelta|TG 16:0_18:1(9Z)_22:6(4Z,7Z,10Z,13Z,16Z,19Z)|] `shouldBe` "TG 16:0_18:1(9Z)_22:6(4Z,7Z,10Z,13Z,16Z,19Z)"
    it "QuasiQuoter for DG 14:0_15:0" $
      shorthand @ (DG DeltaPosition) [dgDelta|DG 14:0_15:0|] `shouldBe` "DG 14:0_15:0"
    it "QuasiQuoter for MG 22:6(4Z,7Z,10Z,13Z,16Z,19Z)" $
      shorthand [mgMaybeDelta|MG 22:6(4Z,7Z,10Z,13Z,16Z,19Z)|] `shouldBe` "MG 22:6(4Z,7Z,10Z,13Z,16Z,19Z)"
  describe "Test for quasiquoters and NNomenclature instances" $ do
    it "QuasiQuoter for TG 16:0_18:1_22:6" $
      nNomenclature @ (TG (Maybe OmegaPosition)) [tgMaybeOmega|TG 16:0_18:1_22:6|] `shouldBe` "TG 16:0_18:1_22:6"
    it "QuasiQuoter for DG 16:0_22:6" $
      nNomenclature @ (DG (Maybe OmegaPosition)) [dgMaybeOmega|DG 16:0_22:6|] `shouldBe` "DG 16:0_22:6"
    it "QuasiQuoter for TG 16:0_18:1(n-9)_22:6" $
      nNomenclature [tgMaybeOmega|TG 16:0_18:1(n-9)_22:6|] `shouldBe` "TG 16:0_18:1(n-9)_22:6"
    it "QuasiQuoter for TG 16:0_18:1(n-9)_18:2(n-9,?)" $
      nNomenclature [tgMaybeOmega|TG 16:0_18:1(n-9)_18:2(n-9,?)|] `shouldBe` "TG 16:0_18:1(n-9)_18:2(n-9,?)"
    it "QuasiQuoter for TG 16:0_18:1(n-9)_18:2(n-9,n-6)" $
      nNomenclature [tgOmega|TG 16:0_18:1(n-9)_18:2(n-9,n-6)|] `shouldBe` "TG 16:0_18:1(n-9)_18:2(n-6)"
    it "QuasiQuoter for DG 14:0_15:0" $
      nNomenclature @ (DG OmegaPosition) [dgOmega|DG 14:0_15:0|] `shouldBe` "DG 14:0_15:0"
    it "QuasiQuoter for MG 22:6(n-3)" $
      nNomenclature [mgMaybeOmega|MG 22:6(n-3)|] `shouldBe` "MG 22:6(n-3)"
    it "QuasiQuoter for MG 22:6(n-3)" $
      nNomenclature [mgOmega|MG 22:6(n-3)|] `shouldBe` "MG 22:6(n-3)"
