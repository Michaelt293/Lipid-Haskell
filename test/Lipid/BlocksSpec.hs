module Lipid.BlocksSpec (spec) where

import Lipid.Blocks
import Test.Hspec

fa_18_3 :: CarbonChain DeltaPosition
fa_18_3 = CarbonChain (NumCarbons 18) [ DoubleBond (DeltaPosition 9)  (Just Cis)
                                      , DoubleBond (DeltaPosition 12) (Just Cis)
                                      , DoubleBond (DeltaPosition 15) (Just Cis)
                                      ]

spec :: Spec
spec = do
  describe "NNomenclature OmegaPosition" $ do
    it "OmegaPosition 9 should be \"n-9\"" $
      nNomenclature (OmegaPosition 9)
      `shouldBe` "n-9"
    it "Just (OmegaPosition 9) should be \"n-9\"" $
      nNomenclature (Just (OmegaPosition 9))
      `shouldBe` "n-9"
    it "Nothing should be \"?\"" $
      nNomenclature (Nothing :: Maybe OmegaPosition)
      `shouldBe` "?"

  describe "Shorthand (DoubleBond a)" .
    it "DeltaPosition 9 Cis should be \"9Z\"" $
      shorthand (DoubleBond (DeltaPosition 9) (Just Cis))
      `shouldBe` "9Z"

  describe "NNomenclature (DoubleBond a)" .
    it "OmegaPosition 9 Cis should be \"n-9\"" $
      nNomenclature (DoubleBond (OmegaPosition 9) (Just Cis))
      `shouldBe` "n-9"

  -- describe "Shorthand (MoietyData a)" .
  --   it "OmegaPosition 9 Cis should be \"9Me\"" $
  --     shorthand (MoietyData (DeltaPosition 9) Methyl)
  --     `shouldBe` "9Me"

  describe "Shorthand (CarbonChain a)" .
    it "18:3(9Z,12Z,15Z)" $
      shorthand fa_18_3
      `shouldBe` "18:3(9Z,12Z,15Z)"

  describe "IsBisAllylic [DoubleBond DeltaPosition]" $ do
    it "Delta 9, 12, 15 should be True" $
      isBisAllylic [ DoubleBond (DeltaPosition 9)  (Just Cis)
                   , DoubleBond (DeltaPosition 12) (Just Cis)
                   , DoubleBond (DeltaPosition 15) (Just Cis)
                   ]
      `shouldBe` Just True
    it "Delta 9 should be False" $
      isBisAllylic [ DoubleBond (DeltaPosition 9)  (Just Cis)]
      `shouldBe` Just False
    it "Delta 9, 12, 16 should be False" $
      isBisAllylic [ DoubleBond (DeltaPosition 9)  (Just Cis)
                   , DoubleBond (DeltaPosition 12) (Just Cis)
                   , DoubleBond (DeltaPosition 16) (Just Cis)
                   ]
      `shouldBe` Just False

  describe "IsBisAllylic [DoubleBond (Maybe DeltaPosition)]" $ do
    it "Delta 9, 12, 15 should be True" $
      isBisAllylic [ DoubleBond (Just (DeltaPosition 9))  (Just Cis)
                   , DoubleBond (Just (DeltaPosition 12)) (Just Cis)
                   , DoubleBond (Just (DeltaPosition 15)) (Just Cis)
                   ]
        `shouldBe` Just True
    it "Delta 9 should be False" $
      isBisAllylic [ DoubleBond (Just (DeltaPosition 9))  (Just Cis)]
        `shouldBe` Just False
    it "Delta 9, 12, 16 should be False" $
      isBisAllylic [ DoubleBond (Just (DeltaPosition 9))  (Just Cis)
                   , DoubleBond (Just (DeltaPosition 12)) (Just Cis)
                   , DoubleBond (Just (DeltaPosition 16)) (Just Cis)
                   ]
       `shouldBe` Just False
    it "Delta 9, ?, 15 should be False" $
      isBisAllylic [ DoubleBond (Just (DeltaPosition 9))  (Just Cis)
                   , DoubleBond Nothing (Just Cis)
                   , DoubleBond (Just (DeltaPosition 15)) (Just Cis)
                   ]
       `shouldBe` Nothing

  describe "renderOmegaPositions" $ do
    it "" $
      renderOmegaPositions [ DoubleBond (OmegaPosition 3) (Just Cis)
                           , DoubleBond (OmegaPosition 6) (Just Cis)
                           , DoubleBond (OmegaPosition 9) (Just Cis)
                           ]
        `shouldBe` "(n-3)"
    it "" $
      renderOmegaPositions [ DoubleBond (OmegaPosition 3) (Just Cis)
                           , DoubleBond (OmegaPosition 7) (Just Cis)
                           , DoubleBond (OmegaPosition 9) (Just Cis)
                           ]
        `shouldBe` "(n-3,n-7,n-9)"
    it "Delta 9, ?, 15 should be (n-9,?,n-15)" $
      renderMaybeOmegaPositions [ DoubleBond (Just (OmegaPosition 9))  (Just Cis)
                           , DoubleBond Nothing (Just Cis)
                           , DoubleBond (Just (OmegaPosition 15)) (Just Cis)
                           ]
       `shouldBe` "(n-9,?,n-15)"
