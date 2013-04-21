{-# LANGUAGE OverloadedStrings #-}
{- Simply typed lambda calculus, from chapter 9 -}
module Stlc (
      module Stlc.Parse
    , module Stlc.Interpret
    ) where

import Stlc.Parse
import Stlc.Interpret
