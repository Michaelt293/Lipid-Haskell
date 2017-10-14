{-|
Module      : Lipid.Parsers.KnownSn.GlycerolipidSpec
Description :
Copyright   : Michael Thomas
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental
-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Lipid.Parsers.KnownSn.GlycerolipidSpec where

import Lipid.Blocks
import Test.Hspec
import Lipid.KnownSn.Glycerolipid
import Lipid.Parsers.KnownSn.Glycerolipid

spec :: Spec
spec = do
  describe "Test for quasiquoters and Shorthand instances" $ do
    it "QuasiQuoter for TG 16:0/18:1/22:6" $
      shorthand @ (TG (Maybe DeltaPosition)) [tgMaybeDelta|TG 16:0/18:1/22:6|] `shouldBe` "TG 16:0/18:1/22:6"
    it "QuasiQuoter for DG 0:0/16:0/22:6" $
      shorthand @ (DG (Maybe DeltaPosition)) [dgMaybeDelta|DG 0:0/16:0/22:6|] `shouldBe` "DG 0:0/16:0/22:6"
    it "QuasiQuoter for DG 16:0/0:0/22:66" $
      shorthand @ (DG (Maybe DeltaPosition)) [dgMaybeDelta|DG 16:0/0:0/22:6|] `shouldBe` "DG 16:0/0:0/22:6"
    it "QuasiQuoter for DG 16:0/22:6/0:0" $
      shorthand @ (DG (Maybe DeltaPosition)) [dgMaybeDelta|DG 16:0/22:6/0:0|] `shouldBe` "DG 16:0/22:6/0:0"
    it "QuasiQuoter for TG 16:0/18:1(9Z)/22:6" $
      shorthand [tgMaybeDelta|TG 16:0/18:1(9Z)/22:6|] `shouldBe` "TG 16:0/18:1(9Z)/22:6"
    it "QuasiQuoter for TG 16:0/18:1(9Z)/22:6(4,7,10,?,16,19)" $
      shorthand [tgMaybeDelta|TG 16:0/18:1(9Z)/22:6(4,7,10,?,16,19)|] `shouldBe` "TG 16:0/18:1(9Z)/22:6(4,7,10,?,16,19)"
    it "QuasiQuoter for TG 16:0/18:1(9Z)/22:6(4Z,7Z,10Z,13Z,16Z,19Z)" $
      shorthand [tgDelta|TG 16:0/18:1(9Z)/22:6(4Z,7Z,10Z,13Z,16Z,19Z)|] `shouldBe` "TG 16:0/18:1(9Z)/22:6(4Z,7Z,10Z,13Z,16Z,19Z)"
    it "QuasiQuoter for DG 0:0/14:0/15:0" $
      shorthand @ (DG DeltaPosition) [dgDelta|DG 0:0/14:0/15:0|] `shouldBe` "DG 0:0/14:0/15:0"
  describe "Test for quasiquoters and NNomenclature instances" $ do
    it "QuasiQuoter for TG 16:0/18:1/22:6|" $
      nNomenclature @ (TG (Maybe OmegaPosition)) [tgMaybeOmega|TG 16:0/18:1/22:6|] `shouldBe` "TG 16:0/18:1/22:6"
    it "QuasiQuoter for DG 0:0/16:0/22:6" $
      nNomenclature @ (DG (Maybe OmegaPosition)) [dgMaybeOmega|DG 0:0/16:0/22:6|] `shouldBe` "DG 0:0/16:0/22:6"
    it "QuasiQuoter for DG 16:0/0:0/22:66" $
      nNomenclature @ (DG (Maybe OmegaPosition)) [dgMaybeOmega|DG 16:0/0:0/22:6|] `shouldBe` "DG 16:0/0:0/22:6"
    it "QuasiQuoter for DG 16:0/22:6/0:0" $
      nNomenclature @ (DG (Maybe OmegaPosition)) [dgMaybeOmega|DG 16:0/22:6/0:0|] `shouldBe` "DG 16:0/22:6/0:0"
    it "QuasiQuoter for TG 16:0/18:1(n-9)/22:6" $
      nNomenclature [tgMaybeOmega|TG 16:0/18:1(n-9)/22:6|] `shouldBe` "TG 16:0/18:1(n-9)/22:6"
    it "QuasiQuoter for TG 16:0/18:1(n-9)/18:2(n-9,?)" $
      nNomenclature [tgMaybeOmega|TG 16:0/18:1(n-9)/18:2(n-9,?)|] `shouldBe` "TG 16:0/18:1(n-9)/18:2(n-9,?)"
    it "QuasiQuoter for TG 16:0/18:1(n-9)/18:2(n-9,n-6)" $
      nNomenclature [tgOmega|TG 16:0/18:1(n-9)/18:2(n-9,n-6)|] `shouldBe` "TG 16:0/18:1(n-9)/18:2(n-6)"
    it "QuasiQuoter for DG 12:0/14:0/15:0" $
      nNomenclature @ (DG OmegaPosition) [dgOmega|DG 0:0/14:0/15:0|] `shouldBe` "DG 0:0/14:0/15:0"
