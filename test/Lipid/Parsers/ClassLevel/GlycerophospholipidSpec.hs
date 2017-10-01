{-|
Module      : Lipid.ClassLevel.Parsers.GlycerophosholipidSpec
Description :
Copyright   : Michael Thomas
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Lipid.Parsers.ClassLevel.GlycerophospholipidSpec where

import Prelude hiding (pi)
import Lipid.Blocks
import Test.Hspec
import Lipid.ClassLevel.Glycerolipid
import Lipid.Parsers.ClassLevel.Glycerophospholipid

spec :: Spec
spec =
  describe "Test for quasiquoters and Shorthand instances" $ do
    it "QuasiQuoter for PA (830)" $
      shorthand [pa|PA (830)|] `shouldBe` "PA (830)"
    it "QuasiQuoter for PE (630)" $
      shorthand [pe|PE (630)|] `shouldBe` "PE (630)"
    it "QuasiQuoter for PC (430)" $
      shorthand [pc|PC (430)|] `shouldBe` "PC (430)"
    it "QuasiQuoter for PG (830)" $
      shorthand [pg|PG (830)|] `shouldBe` "PG (830)"
    it "QuasiQuoter for PGP (630)" $
      shorthand [pgp|PGP (630)|] `shouldBe` "PGP (630)"
    it "QuasiQuoter for PS (430)" $
      shorthand [ps|PS (430)|] `shouldBe` "PS (430)"
    it "QuasiQuoter for PI (830)" $
      shorthand [pi|PI (830)|] `shouldBe` "PI (830)"
    it "QuasiQuoter for PIP3 (630)" $
      shorthand [pip3|PIP3 (630)|] `shouldBe` "PIP3 (630)"
    it "QuasiQuoter for PIP[3′] (430)" $
      shorthand [pip|PIP[3′] (430)|] `shouldBe` "PIP[3′] (430)"
    it "QuasiQuoter for PIP[4′] (430)" $
      shorthand [pip|PIP[4′] (430)|] `shouldBe` "PIP[4′] (430)"
    it "QuasiQuoter for PIP[5′] (430)" $
      shorthand [pip|PIP[5′] (430)|] `shouldBe` "PIP[5′] (430)"
    it "QuasiQuoter for PIP2[3′,4′] (430)" $
      shorthand [pip2|PIP2[3′,4′] (430)|] `shouldBe` "PIP2[3′,4′] (430)"
    it "QuasiQuoter for PIP2[4′,5′] (430)" $
      shorthand [pip2|PIP2[4′,5′] (430)|] `shouldBe` "PIP2[4′,5′] (430)"
    it "QuasiQuoter for PIP2[3′,5′] (430)" $
      shorthand [pip2|PIP2[3′,5′] (430)|] `shouldBe` "PIP2[3′,5′] (430)"
