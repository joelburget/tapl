{-# LANGUAGE OverloadedStrings #-}
module Untyped where

import Data.Monoid ((<>))
import Data.Text hiding (length)
import Data.Text.Format
import Data.Text.Lazy (toStrict)
import Safe
import Text.Printf

data Info = Info deriving Show

data Term
    = TmVar Info Int Int
    | TmAbs Info Text Term
    | TmApp Info Term Term
    deriving Show

data Binding = NameBind deriving Show

type Context = [(Text, Binding)]

-- | Print a term with named variables
printtm :: Context -> Term -> Text
printtm ctx (TmAbs _ x t1) = let (ctx', x') = pickFreshName ctx x in
    toStrict $ format "(lambda {}. {})" (x', printtm ctx' t1)
printtm ctx (TmApp _ t1 t2) =
    toStrict $ format "({} {})" (printtm ctx t1, printtm ctx t2)
printtm ctx (TmVar fi x n) = if ctxlength ctx == n
    then index2name fi ctx x else toStrict $ format "[bad index {}/{}]" (ctxlength ctx, n)

-- | Number of name bindings
ctxlength :: Context -> Int
ctxlength = length

isNameBound :: Context -> Text -> Bool
isNameBound ctx x = case ctx of
    [] -> False
    (y, _):rest -> x == y || isNameBound rest x

-- | Keep appending ' to the given name until it is new and add it to the
-- context
pickFreshName :: Context -> Text -> (Context, Text)
pickFreshName ctx x = if isNameBound ctx x
    then pickFreshName ctx (x <> "'")
    else ((x, NameBind):ctx, x)

index2name :: Info -> Context -> Int -> Text
index2name _ ctx x = fst $ atNote
    (printf "Variable lookup failure: offset: %d, ctx size: %d" x (length ctx))
    ctx x

name2index :: Info -> Context -> Text -> Int
name2index fi ctx x = case ctx of
    [] -> error (printf "Identifier %s is unbound" (show x))
    (y, _):rest -> if x == y then 0 else 1 + name2index fi rest x

tmMap :: (Info -> Int -> Int -> Int -> Term) -> Int -> Term -> Term
tmMap onvar c t = let
    walk c' t' = case t' of
        TmVar fi x n -> onvar fi c' x n
        TmAbs fi x t2 -> TmAbs fi x (walk (c'+1) t2)
        TmApp fi t1 t2 -> TmApp fi (walk c' t1) (walk c' t2)
    in walk c t

termShiftAbove :: Int -> Int -> Term -> Term
termShiftAbove d = tmMap (\fi c' x n ->
    if x >= c' then TmVar fi (x+d) (n+d) else TmVar fi x (n+d))

termShift :: Int -> Term -> Term
termShift d = termShiftAbove d 0

termSubst :: Int -> Term -> Term -> Term
termSubst j s = tmMap (\fi c x n ->
        if x == j + c then termShift c s else TmVar fi x n)
    0

termSubstTop :: Term -> Term -> Term
termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)

isVal :: Context -> Term -> Bool
isVal _ TmAbs{} = True
isVal _ _ = False

eval1 :: Context -> Term -> Maybe Term
eval1 ctx (TmApp _ (TmAbs _ _ t12) v2) | isVal ctx v2 = Just $
    termSubstTop v2 t12
eval1 ctx (TmApp fi v1 t2) | isVal ctx v1 = TmApp fi v1 `fmap` eval1 ctx t2
eval1 ctx (TmApp fi t1 t2) = (\x -> TmApp fi x t2) `fmap` eval1 ctx t1
eval1 _ _ = Nothing

eval :: Context -> Term -> Term
eval ctx t = case eval1 ctx t of
    Just t' -> eval ctx t'
    Nothing -> t
