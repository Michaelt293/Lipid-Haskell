{-|
Module      : Lipid.Format
Description : Functions for formatting lipid shorthand.
Copyright   : Michael Thomas
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental
-}

module Lipid.Format
    ( wrap
    , wrapParen
    , wrapBrackets
    ) where

import Data.Monoid ((<>))

wrap :: String -> String -> String -> String
wrap open close str =
  if null str
    then ""
    else open <> str <> close

wrapParen :: String -> String
wrapParen = wrap "(" ")"

wrapBrackets :: String -> String
wrapBrackets = wrap "[" "]"
