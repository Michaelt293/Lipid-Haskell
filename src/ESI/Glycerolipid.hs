{-|
Module      : ESI.Glycerolipid
Description : posESI and negESI methods for glycerolipids. 
Copyright   : Michael Thomas 
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental
-}

module ESI.Glycerolipid where

import Lipid.Lipid
import Lipid.Glycerolipid
import Ion.LipidIon

instance Lipid MG where
    posESI mg = Just (MGion mg 1 [])
    negESI mp = Nothing

instance Lipid DG where
    posESI dg = Just (DGion dg 1 [])
    negESI dg = Nothing

instance Lipid TG where
    posESI tg = Just (TGion tg 1 [])
    negESI tg = Nothing