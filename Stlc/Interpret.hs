{-# LANGUAGE OverloadedStrings #-}
{- Simply typed lambda calculus, from chapter 10 -}
module Stlc.Interpret where

import Data.Monoid ((<>))
import Data.Text hiding (length)
import Data.Text.Format
import Data.Text.Lazy (toStrict)
import Safe
import Text.Printf

data Ty
    = TyBool
    | TyArr Ty Ty
    deriving (Eq, Show)

data Info = Info deriving (Eq, Show)

data Term
    = TmTrue Info
    | TmFalse Info
    | TmIf Info Term Term Term
    | TmVar Info Int Int
    | TmAbs Info Text Ty Term
    | TmApp Info Term Term
    deriving (Eq, Show)

data Binding
    = NameBind
    | VarBind Ty
    deriving Show

type Context = [(Text, Binding)]

-- | Print a term with named variables
printtm :: Context -> Term -> Text
printtm ctx (TmTrue _) = "True"
printtm ctx (TmFalse _) = "False"
printtm ctx (TmIf _ test b1 b2) = toStrict $ format "if {} then {} else {}"
    (printtm ctx test, printtm ctx b1, printtm ctx b2)
printtm ctx (TmVar fi x n) = if ctxlength ctx == n
    then index2name fi ctx x else toStrict $ format "[bad index {}/{}]" (ctxlength ctx, n)
printtm ctx (TmAbs _ x ty t1) = let (ctx', x') = pickFreshName ctx x in
    toStrict $ format "(lambda {}:{}. {})" (x', show ty, printtm ctx' t1)
printtm ctx (TmApp _ t1 t2) =
    toStrict $ format "({} {})" (printtm ctx t1, printtm ctx t2)

-- | Number of name bindings
ctxlength :: Context -> Int
ctxlength = length

--

addBinding :: Context -> Text -> Binding -> Context
addBinding ctx x bind = (x, bind):ctx

getTypeFromContext :: Info -> Context -> Int -> Ty
getTypeFromContext fi ctx i = case getBinding fi ctx i of
    VarBind tyT -> tyT
    _ -> error $
        printf "getTypeFromContext: Wrong kind of binding for variable %s"
               (show $ index2name fi ctx i)

typeof :: Context -> Term -> Ty
typeof ctx t = case t of
    TmTrue fi -> TyBool
    TmFalse fi -> TyBool
    TmIf fi t1 t2 t3 -> if (==) (typeof ctx t1) TyBool
        then let tyT2 = typeof ctx t2 in
                     if (==) tyT2 (typeof ctx t3)
                     then tyT2
                     else error "arms of conditional have different types"
        else error "guard of conditional not a boolean"
    TmVar fi i _ -> getTypeFromContext fi ctx i
    TmAbs fi x tyT1 t2 ->
        let ctx' = addBinding ctx x (VarBind tyT1)
            tyT2 = typeof ctx' t2
        in TyArr tyT1 tyT2
    TmApp fi t1 t2 ->
        let tyT1 = typeof ctx t1
            tyT2 = typeof ctx t2
        in case tyT1 of
            TyArr tyT11 tyT12 -> if (==) tyT2 tyT11
                then tyT12
                else error "parameter type mismatch"
            _ -> error "arrow type expected"

--

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
index2name _ ctx i = fst $ varLookup ctx i

getBinding :: Info -> Context -> Int -> Binding
getBinding _ ctx i = snd $ varLookup ctx i

varLookup :: Context -> Int -> (Text, Binding)
varLookup ctx i = atNote
    (printf "Variable lookup failure: offset: %d, ctx size: %d" i (length ctx))
    ctx i

name2index :: Info -> Context -> Text -> Int
name2index fi ctx x = case ctx of
    [] -> error (printf "Identifier %s is unbound" (show x))
    (y, _):rest -> if x == y then 0 else 1 + name2index fi rest x

tmMap :: (Info -> Int -> Int -> Int -> Term) -> Int -> Term -> Term
tmMap onvar c t = let
    walk c' t' = case t' of
        TmVar fi x n -> onvar fi c' x n
        TmAbs fi x ty t2 -> TmAbs fi x ty (walk (c'+1) t2)
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
eval1 ctx (TmApp _ (TmAbs _ _ _ t12) v2) | isVal ctx v2 = Just $
    termSubstTop v2 t12
eval1 ctx (TmApp fi v1 t2) | isVal ctx v1 = TmApp fi v1 `fmap` eval1 ctx t2
eval1 ctx (TmApp fi t1 t2) = (\x -> TmApp fi x t2) `fmap` eval1 ctx t1
eval1 _ _ = Nothing

eval :: Context -> Term -> Term
eval ctx t = case eval1 ctx t of
    Just t' -> eval ctx t'
    Nothing -> t
