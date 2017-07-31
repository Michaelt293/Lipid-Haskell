{-|
Module      : Lipid.Format
Description : Functions for formatting lipid shorthand.
Copyright   : Michael Thomas
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental
-}

module Lipid.Format
    (
      wrap
    , wrapParen
    , wrapBrackets
    ) where

wrap :: [Char] -> [Char] -> [Char] -> [Char]
wrap open close str =
  if null str
    then ""
    else open ++ str ++ close

wrapParen :: [Char] -> [Char]
wrapParen str = wrap "(" ")" str

wrapBrackets :: [Char] -> [Char]
wrapBrackets str = wrap "[" "]" str
