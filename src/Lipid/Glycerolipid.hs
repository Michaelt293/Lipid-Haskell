{-|
Module      : Lipid.Glycerolipid
Description : Glycerolipid data type and instances of Shorthand and
              Nomenclature defined.
Copyright   : Michael Thomas
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental
-}
{-# LANGUAGE TemplateHaskell #-}

module Lipid.Glycerolipid
    (
      MG(..)
    , DG(..)
    , TG(..)
    ) where

import Lipid.Blocks
import Lipid.Format
import Control.Lens


data MG a b
  = ClassLevelMG Integer
  | UnknownSn    (Radyl a b)
  | Sn1MG        (Radyl a b)
  | Sn2MG        (Radyl a b)
  | Sn3MG        (Radyl a b)
  deriving (Show, Eq, Ord)

makePrisms ''MG

data DG a b
  = ClassLevelDG     Integer
  | CombinedRadylsDG (TwoCombinedRadyls b)
  | UnknownDG        (Radyl a b) (Radyl a b)
  | Sn12DG           (Radyl a b) (Radyl a b)
  | Sn13DG           (Radyl a b) (Radyl a b)
  | Sn23DG           (Radyl a b) (Radyl a b)
  deriving (Show, Eq, Ord)

makePrisms ''DG

data TG a b
  = ClassLevelTG     Integer
  | CombinedRadylsTG (ThreeCombinedRadyls b)
  | UnknownSnTG      (Radyl a b) (Radyl a b) (Radyl a b)
  | KnownSnTG        (Radyl a b) (Radyl a b) (Radyl a b)
  deriving (Show, Eq, Ord)

makePrisms ''TG

-- instance Shorthand MG where
--     showShorthand l =
--       case l of
--         (ClassLevelMG n) -> "MG (" <> show n <> ")"
--         (UnknownSn r)    -> "MG " <> showShorthand r
--         (Sn1MG r)        -> "MG " <> showShorthand r <> "/0:0/0:0"
--         (Sn2MG r)        -> "MG 0:0/" <> showShorthand r <> "/0:0"
--         (Sn3MG r)        -> "MG 0:0/0:0/" <> showShorthand r
--
-- instance Nomenclature MG where
--     showNnomenclature l =
--       case l of
--         (ClassLevelMG n) -> "MG (" <> show n <> ")"
--         (UnknownSn r)    -> "MG " <> showNnomenclature r
--         (Sn1MG r)        -> "MG " <> showNnomenclature r <> "/0:0/0:0"
--         (Sn2MG r)        -> "MG 0:0/" <> showNnomenclature r <> "/0:0"
--         (Sn3MG r)        -> "MG 0:0/0:0/" <> showNnomenclature r
--
-- instance Shorthand DG where
--     showShorthand l =
--       case l of
--         (ClassLevelDG n)      -> "DG (" <> show n <> ")"
--         (CombinedRadylsDG rs) -> "DG " <> showShorthand rs
--         (UnknownDG r1 r2)     -> renderDG showShorthand "unknown" "_" r1 r2
--         (Sn12DG r1 r2)        -> renderDG showShorthand "12" "/" r1 r2
--         (Sn13DG r1 r2)        -> renderDG showShorthand "13" "/" r1 r2
--         (Sn23DG r1 r2)        -> renderDG showShorthand "23" "/" r1 r2
--
-- instance Nomenclature DG where
--     showNnomenclature l =
--       case l of
--         (ClassLevelDG n)     -> "DG (" <> show n <> ")"
--         (CombinedRadylsDG rs) -> "DG " <> showNnomenclature rs
--         (UnknownDG r1 r2)    -> renderDG showNnomenclature "unknown" "_" r1 r2
--         (Sn12DG r1 r2)       -> renderDG showNnomenclature "12" "/" r1 r2
--         (Sn13DG r1 r2)       -> renderDG showNnomenclature "13" "/" r1 r2
--         (Sn23DG r1 r2)       -> renderDG showNnomenclature "23" "/" r1 r2
--
-- renderDG f sn sep r1 r2 =
--   case sn of
--     "unknown" -> "DG " <> r1' <> sep <> r2'
--     "12" -> "DG " <> r1' <> sep <> r2' <> sep <> "0:0"
--     "13" -> "DG " <> r1' <> sep <> "0:0" <> sep <> r2'
--     "23" -> "DG 0:0" <> sep <> r1' <> sep <> r2'
--     where r1' = f r1
--           r2' = f r2
--
-- instance Shorthand TG where
--   showShorthand l =
--     case l of
--       (ClassLevelTG n)       -> "TG (" <> show n <> ")"
--       (CombinedRadylsTG rs)  -> "TG " <> showShorthand rs
--       (UnknownSnTG r1 r2 r3) -> renderTG showNnomenclature "_" r1 r2 r3
--       (KnownSnTG r1 r2 r3)   -> renderTG showShorthand "/" r1 r2 r3
--
-- instance Nomenclature TG where
--   showNnomenclature l =
--     case l of
--       (ClassLevelTG n)       -> "TG (" <> show n <> ")"
--       (CombinedRadylsTG rs)  -> "TG " <> showNnomenclature rs
--       (UnknownSnTG r1 r2 r3) -> renderTG showNnomenclature "_" r1 r2 r3
--       (KnownSnTG r1 r2 r3)   -> renderTG showNnomenclature "/" r1 r2 r3
--
-- renderTG f sep r1 r2 r3  = "TG " <> r1' <> sep <> r2' <> sep <> r3'
--     where r1' = f r1
--           r2' = f r2
--           r3' = f r3
