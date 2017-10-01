{-|
Module      : Lipid.ClassLevel.Parsers.Glycerolipid
Description :
Copyright   : Michael Thomas
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Lipid.Parsers.ClassLevel.GlycerolipidSpec where

import Lipid.Blocks
import Test.Hspec
import Lipid.ClassLevel.Glycerolipid
import Lipid.Parsers.ClassLevel.Glycerolipid


spec :: Spec
spec =
  describe "Test for quasiquoters and Shorthand instances" $ do
    it "QuasiQuoter for TG (830)" $
      shorthand [tg|TG (830)|] `shouldBe` "TG (830)"
    it "QuasiQuoter for DG (630)" $
      shorthand [dg|DG (630)|] `shouldBe` "DG (630)"
    it "QuasiQuoter for MG (430)" $
      shorthand [mg|MG (430)|] `shouldBe` "MG (430)"
