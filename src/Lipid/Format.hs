{-|
Module      : Format
Description : Functions for formatting lipid shorthand.
Copyright   : Copyright 2015 IMBCR pty ltd. All rights reserved.
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : experimental
-}

module Lipid.Format
    (
      wrap 
    , wrapParen 
    , wrapBrackets
    ) where

import qualified Data.List as List


wrap :: [Char] -> [Char] -> [Char] -> [Char]
wrap open close str = if length str == 0 
                    then ""
                    else open ++ str ++ close

wrapParen :: [Char] -> [Char]
wrapParen str = wrap "(" ")" str

wrapBrackets :: [Char] -> [Char]
wrapBrackets str = wrap "[" "]" str

