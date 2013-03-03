{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Identity (Identity)
import Control.Wire
import Data.Monoid ((<>))
import Data.Text as T
import Data.Text.IO as T
import Prelude hiding ((.), id)
import qualified Text.Parsec as P
import Text.Parsec.Text ()

import Parse2
import Untyped

evaluate :: Wire P.ParseError IO Text Text
evaluate = mkState [] $ \_ (input, ctx) -> case parse command ctx input of
    Left err -> (Left err, ctx)
    Right (cmd, ctx') -> (Right (printcmd ctx' cmd), ctx')

tostr :: Monad m => Wire e m Text String
tostr = arr show

main :: IO ()
main = testWire 1 10 T.getLine clockSession (evaluate >>> tostr)
