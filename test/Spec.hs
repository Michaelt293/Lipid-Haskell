
module Main where

import Test.Hspec
import Lipid.BlocksSpec
import Lipid.FattyAcidSpec
import Lipid.GlycerolipidSpec
import Lipid.Parsers.FattyAcidSpec
import Lipid.Parsers.ClassLevel.GlycerolipidSpec
import Lipid.Parsers.ClassLevel.GlycerophospholipidSpec
import Lipid.Parsers.CombinedRadyl.GlycerolipidSpec
import Lipid.Parsers.CombinedRadyl.GlycerophospholipidSpec
import Lipid.Parsers.KnownSn.GlycerolipidSpec
import Lipid.Parsers.UnknownSn.GlycerolipidSpec
import Lipid.Parsers.KnownSn.GlycerophospholipidSpec
import Lipid.Parsers.UnknownSn.GlycerophospholipidSpec


main :: IO ()
main = hspec $ do
  describe "Blocks" Lipid.BlocksSpec.spec
  describe "FattyAcid" Lipid.FattyAcidSpec.spec
  describe "Glycerolipid" Lipid.GlycerolipidSpec.spec
  describe "Class level glycerolipid QuasiQuoter"
    Lipid.Parsers.ClassLevel.GlycerolipidSpec.spec
  describe "Class level Glycerophospholipid QuasiQuoter"
    Lipid.Parsers.ClassLevel.GlycerophospholipidSpec.spec
  describe "CombinedRadyl level Glycerolipid QuasiQuoter"
    Lipid.Parsers.CombinedRadyl.GlycerolipidSpec.spec
  describe "CombinedRadyl level Glycerophospholipid QuasiQuoter"
    Lipid.Parsers.CombinedRadyl.GlycerophospholipidSpec.spec
  describe "KnownSn level Glycerolipid QuasiQuoter"
    Lipid.Parsers.KnownSn.GlycerolipidSpec.spec
  describe "KnownSn level Glycerophospholipid QuasiQuoter"
    Lipid.Parsers.KnownSn.GlycerophospholipidSpec.spec
  describe "UnknownSn level Glycerolipid QuasiQuoter"
    Lipid.Parsers.UnknownSn.GlycerolipidSpec.spec
  describe "UnknownSn level Glycerophospholipid QuasiQuoter"
    Lipid.Parsers.UnknownSn.GlycerophospholipidSpec.spec
  describe "FattyAcid QuasiQuoter"
    Lipid.Parsers.FattyAcidSpec.spec
