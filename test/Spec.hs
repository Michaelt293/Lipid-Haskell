
module Main where

import Test.Hspec
import Lipid.BlocksSpec
import Lipid.FattyAcidSpec
import Lipid.GlycerolipidSpec
import Lipid.Parsers.ClassLevel.GlycerolipidSpec

main :: IO ()
main = hspec $ do
  describe "Blocks" Lipid.BlocksSpec.spec
  describe "FattyAcid" Lipid.FattyAcidSpec.spec
  describe "Glycerolipid" Lipid.GlycerolipidSpec.spec
  describe "Class level glycerolipid QuasiQuoter" Lipid.Parsers.ClassLevel.GlycerolipidSpec.spec
