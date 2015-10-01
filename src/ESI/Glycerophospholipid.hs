{-|
Module      : ESI.Glycerophospholipid
Description : posESI and negESI methods for glycerophospholipids.
Copyright   : Michael Thomas 
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : experimental
-}

module ESI.Glycerophospholipid where

import Lipid.Lipid
import Lipid.Glycerophospholipid
import Ion.LipidIon

instance Lipid PA where
    posESI pa = Just (PAion pa 1 [])
    negESI pa = Just (PAion pa (-1) [])

instance Lipid PC where
    posESI pc = Just (PCion pc 1 [])
    negESI pc = Nothing

instance Lipid PE where
    posESI pe = Just (PEion pe 1 [])
    negESI pe = Just (PEion pe (-1) [])

instance Lipid PG where
    posESI pg = Just (PGion pg 1 [])
    negESI pg = Just (PGion pg (-1) [])

instance Lipid PGP where
    posESI pgp = Just (PGPion pgp 1 [])
    negESI pgp = Just (PGPion pgp (-2) [])

instance Lipid PI where
    posESI pi = Just (PIion pi 1 [])
    negESI pi = Just (PIion pi (-1) [])

instance Lipid PIP where
    posESI pip = Just (PIPion pip 1 [])
    negESI pip = Just (PIPion pip (-2) [])

instance Lipid PIP2 where
    posESI pip2 = Just (PIP2ion pip2 1 [])
    negESI pip2 = Just (PIP2ion pip2 (-2) [])

instance Lipid PIP3 where
    posESI pip3 = Just (PIP3ion pip3 1 [])
    negESI pip3 = Just (PIP3ion pip3 (-3) [])

instance Lipid PS where
    posESI ps = Just (PSion ps 1 [])
    negESI ps = Just (PSion ps (-1) [])

instance Lipid CL where
    posESI cl = Just (CLion cl 1 [])
    negESI cl = Just (CLion cl (-2) [])

instance Lipid BMP where
    posESI bmp = Just (BMPion bmp 1 [])
    negESI bmp = Just (BMPion bmp (-1) [])

