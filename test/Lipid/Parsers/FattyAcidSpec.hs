{-|
Module      : Lipid.Parsers.FattyAcidSpec
Description :
Copyright   : Michael Thomas
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental
-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Lipid.Parsers.FattyAcidSpec where

import Lipid.Blocks
import Test.Hspec
import Lipid.FattyAcid
import Lipid.Parsers.FattyAcid

spec :: Spec
spec = do
  describe "Test for quasiquoters and Shorthand instances" $ do
    it "QuasiQuoter for FA (226)" $
      shorthand @ (FA DeltaPosition) [faDelta|FA (226)|] `shouldBe` "FA (226)"
    it "QuasiQuoter for FA 22:6" $
      shorthand @ (FA (Maybe DeltaPosition)) [faMaybeDelta|FA 22:6|] `shouldBe` "FA 22:6"
    it "QuasiQuoter for FA 22:6(4,7,10,?,16,19)" $
      shorthand [faMaybeDelta|FA 22:6(4,7,10,?,16,19)|] `shouldBe` "FA 22:6(4,7,10,?,16,19)"
    it "QuasiQuoter for FA 22:6(4Z,7Z,10Z,13Z,16Z,19Z)" $
      shorthand [faDelta|FA 22:6(4Z,7Z,10Z,13Z,16Z,19Z)|] `shouldBe` "FA 22:6(4Z,7Z,10Z,13Z,16Z,19Z)"
    it "QuasiQuoter for FA 15:0" $
      shorthand @ (FA DeltaPosition) [faDelta|FA 15:0|] `shouldBe` "FA 15:0"
  describe "Test for quasiquoters and NNomenclature instances" $ do
    it "QuasiQuoter for FA 22:6" $
      nNomenclature @ (FA (Maybe OmegaPosition)) [faMaybeOmega|FA 22:6|] `shouldBe` "FA 22:6"
    it "QuasiQuoter for FA 18:2(n-9,?)" $
      nNomenclature [faMaybeOmega|FA 18:2(n-9,?)|] `shouldBe` "FA 18:2(n-9,?)"
    it "QuasiQuoter for FA 18:2(n-6)" $
      nNomenclature [faOmega|FA 18:2(n-6)|] `shouldBe` "FA 18:2(n-6)"
    it "QuasiQuoter for FA 15:0" $
      nNomenclature @ (FA OmegaPosition) [faOmega|FA 15:0|] `shouldBe` "FA 15:0"
