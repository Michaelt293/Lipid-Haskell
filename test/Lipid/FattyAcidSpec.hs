module Lipid.FattyAcidSpec (spec) where

import Lipid.FattyAcid
import Test.Hspec

spec :: Spec
spec = do
  return ()
  -- describe "Elemental composition for FattyAcyl" $ do
  --   it "Stearic acid acyl group elemental composition should be C18H35O2" $
  --     toElementalComposition (FattyAcyl 18 0)
  --     `shouldBe` mkElementalComposition [(C, 18), (H, 35), (O, 2)]
  --   it "Oleic acid acyl group elemental composition should be C18H33O2" $
  --     toElementalComposition (FattyAcyl 18 1)
  --     `shouldBe` mkElementalComposition [(C, 18),(H, 33), (O, 2)]
  --   it "DHA elemental acyl group composition should be C22H31O2" $
  --     toElementalComposition (FattyAcyl 22 6)
  --     `shouldBe` mkElementalComposition [(C, 22),(H, 31), (O, 2)]

    -- describe "isSaturated" $ do
    --   it "Stearic acid is saturated" $
    --     isSaturated (FattyAcid (FattyAcyl 18 0))
    --     `shouldBe` True
    --   it "Oleic acid is not saturated" $
    --     isSaturated (FattyAcid (FattyAcyl 18 1))
    --     `shouldBe` False
    --   it "DHA is not saturated" $
    --     isSaturated (FattyAcid (FattyAcyl 22 6))
    --     `shouldBe` False
    --
    -- describe "isMonounsaturated" $ do
    --   it "Stearic acid is monounsaturated" $
    --     isMonounsaturated (FattyAcid (FattyAcyl 18 0))
    --   `shouldBe` False
    --   it "Oleic acid is not monounsaturated" $
    --   isMonounsaturated (FattyAcid (FattyAcyl 18 1))
    --   `shouldBe` True
    --   it "DHA is not monounsaturated" $
    --   isMonounsaturated (FattyAcid (FattyAcyl 22 6))
    --   `shouldBe` False
    --
    --      describe "isPolyunsaturated" $ do
    --        it "Stearic acid is polyunsaturated" $
    --          isPolyunsaturated (FattyAcid (FattyAcyl 18 0))
    --          `shouldBe` False
    --        it "Oleic acid is not polyunsaturated" $
    --          isPolyunsaturated (FattyAcid (FattyAcyl 18 1))
    --          `shouldBe` False
    --        it "DHA is not polyunsaturated" $
    --          isPolyunsaturated (FattyAcid (FattyAcyl 22 6))
    --          `shouldBe` True
