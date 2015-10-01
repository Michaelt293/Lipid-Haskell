{-|
Module      : ESI.Lysoglycerophospholipid
Description : posESI and negESI methods for lysoglycerophospholipids.
Copyright   : Michael Thomas 
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental
-}

module ESI.Lysoglycerophospholipid where

import Lipid.Lipid
import Lipid.Lysoglycerophospholipid
import Ion.LipidIon

instance Lipid LPA where
    posESI lpa = Just (LPAion lpa 1 [])
    negESI lpa = Just (LPAion lpa (-1) [])

instance Lipid LPC where
    posESI lpc = Just (LPCion lpc 1 [])
    negESI lpc = Nothing

instance Lipid LPE where
    posESI lpe = Just (LPEion lpe 1 [])
    negESI lpe = Just (LPEion lpe (-1) [])

instance Lipid LPG where
    posESI lpg = Just (LPGion lpg 1 [])
    negESI lpg = Just (LPGion lpg (-1) [])
 
instance Lipid LPGP where
    posESI lpgp = Just (LPGPion lpgp 1 [])
    negESI lpgp = Just (LPGPion lpgp (-2) [])

instance Lipid LPI where
    posESI lpi = Just (LPIion lpi 1 [])
    negESI lpi = Just (LPIion lpi (-1) [])
 
instance Lipid LPIP where
    posESI lpip = Just (LPIPion lpip 1 [])
    negESI lpip = Just (LPIPion lpip (-2) [])

instance Lipid LPIP2 where
    posESI lpip2 = Just (LPIP2ion lpip2 1 [])
    negESI lpip2 = Just (LPIP2ion lpip2 (-2) [])

instance Lipid LPIP3 where
    posESI lpip3 = Just (LPIP3ion lpip3 1 [])
    negESI lpip3 = Just (LPIP3ion lpip3 (-2) [])

instance Lipid LPS where
    posESI lps = Just (LPSion lps 1 [])
    negESI lps = Just (LPSion lps (-1) [])

instance Lipid LCL where
    posESI lcl = Just (LCLion lcl 1 [])
    negESI lcl = Just (LCLion lcl (-2) [])

