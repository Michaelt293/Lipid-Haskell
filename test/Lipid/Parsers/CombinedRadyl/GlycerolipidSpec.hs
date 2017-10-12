{-|
Module      : Lipid.Parsers.CombinedRadyl.GlycerolipidSpec
Description :
Copyright   : Michael Thomas
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Lipid.Parsers.CombinedRadyl.GlycerolipidSpec where

import Lipid.Blocks
import Test.Hspec
import Lipid.CombinedRadyl.Glycerolipid
import Lipid.Parsers.CombinedRadyl.Glycerolipid


spec :: Spec
spec = do
  describe "Test for quasiquoters and Shorthand instances" $ do
    it "QuasiQuoter for TG 52:2" $
      shorthand @ (TG (Maybe DeltaPosition)) [tgMaybeDelta|TG 52:2|] `shouldBe` "TG 52:2"
    it "QuasiQuoter for DG 34:0" $
      shorthand @ (DG (Maybe DeltaPosition)) [dgMaybeDelta|DG 34:0|] `shouldBe` "DG 34:0"
    it "QuasiQuoter for TG 52:2(9Z,12Z)" $
      shorthand [tgMaybeDelta|TG 52:2(9Z,12Z)|] `shouldBe` "TG 52:2(9Z,12Z)"
    it "QuasiQuoter for TG 52:2(9,12Z)" $
      shorthand [tgMaybeDelta|TG 52:2(9,12Z)|] `shouldBe` "TG 52:2(9,12Z)"
    it "QuasiQuoter for TG 52:2(?,12Z)" $
      shorthand [tgMaybeDelta|TG 52:2(?,12Z)|] `shouldBe` "TG 52:2(?,12Z)"
    it "QuasiQuoter for TG 52:2(9Z,12Z)" $
      shorthand [tgDelta|TG 52:2(9Z,12Z)|] `shouldBe` "TG 52:2(9Z,12Z)"
    it "QuasiQuoter for DG 34:0" $
      shorthand @ (DG DeltaPosition) [dgDelta|DG 34:0|] `shouldBe` "DG 34:0"
  describe "Test for quasiquoters and NNomenclature instances" $ do
    it "QuasiQuoter for TG 52:2" $
      nNomenclature @ (TG (Maybe OmegaPosition))  [tgMaybeOmega|TG 52:2|] `shouldBe` "TG 52:2"
    it "QuasiQuoter for DG 34:0" $
      nNomenclature @ (DG (Maybe OmegaPosition))  [dgMaybeOmega|DG 34:0|] `shouldBe` "DG 34:0"
    it "QuasiQuoter for TG 52:2(n-6,n-9)" $
      nNomenclature [tgMaybeOmega|TG 52:2(n-6,n-9)|] `shouldBe` "TG 52:2(n-6,n-9)"
    it "QuasiQuoter for TG 52:2(n-6Z,n-9)" $
      nNomenclature [tgMaybeOmega|TG 52:2(n-6Z,n-9)|] `shouldBe` "TG 52:2(n-6,n-9)"
    it "QuasiQuoter for TG 52:2(?,n-9)" $
      nNomenclature [tgMaybeOmega|TG 52:2(?,n-9)|] `shouldBe` "TG 52:2(?,n-9)"
    it "QuasiQuoter for TG 52:2" $
      nNomenclature [tgOmega|TG 52:2(n-6,n-9)|] `shouldBe` "TG 52:2(n-6,n-9)"
    it "QuasiQuoter for DG 34:0" $
      nNomenclature @ (DG OmegaPosition) [dgOmega|DG 34:0|] `shouldBe` "DG 34:0"
