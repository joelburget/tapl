{-# LANGUAGE LambdaCase, MultiWayIf #-}
{-# LANGUAGE DataKinds, TypeOperators #-}

module Main where

import Control.Applicative ((<*>))
import Control.Monad.State
import Safe
import Text.Printf

data Term
    = TmTrue
    | TmFalse
    | TmIf Term Term Term
    | TmVar Int Int
    | TmAbs String Ty Term
    | TmApp Term Term
    deriving (Eq, Show)

data Binding
    = NameBind
    | VarBind Ty
    deriving Show

type Context = [(String, Binding)]

data Ty
    = TyBool
    | TyArr Ty Ty
    | TyId String
    | TyNat
    deriving (Show, Eq)

type Constr = [(Ty, Ty)]

freshVar = do
    x <- get
    put $ x + 1
    return $ "?X_" ++ show x

recon :: Context -> Term -> State Int (Ty, Constr)
recon ctx (TmVar i _) = let tyT = getTypeFromContext ctx i in return (tyT, [])
recon ctx (TmAbs x tyT1 t2) = do
    let ctx' = addBinding ctx x (VarBind tyT1)
    (tyT2, constr2) <- recon ctx' t2
    return (TyArr tyT1 tyT2, constr2)
recon ctx (TmApp t1 t2) = do
    (tyT1, constr1) <- recon ctx t1
    (tyT2, constr2) <- recon ctx t2
    idX <- freshVar
    let newConstr = [(tyT1, TyArr tyT2 (TyId idX))]
    return (TyId idX, concat [constr1, constr2, newConstr])
-- recon ctx TmZero = return (TyNat, [])
-- recon ctx (TmSucc t1) = do
--     (tyT1, constr1) <- recon ctx t1
--     return (TyNat, (tyT1, TyNat):constr1)
-- recon ctx (TmPred t1)
recon _ TmTrue = return (TyBool, [])
recon _ TmFalse = return (TyBool, [])
recon ctx (TmIf t1 t2 t3) = do
    (tyT1, constr1) <- recon ctx t1
    (tyT2, constr2) <- recon ctx t2
    (tyT3, constr3) <- recon ctx t3
    let newConstr = [(tyT1, TyBool), (tyT2, tyT3)]
    return (tyT3, concat [newConstr, constr1, constr2, constr3])

addBinding :: Context -> String -> Binding -> Context
addBinding ctx x bind = (x, bind):ctx

getBinding :: Context -> Int -> Binding
getBinding ctx i = snd $ varLookup ctx i

getTypeFromContext :: Context -> Int -> Ty
getTypeFromContext ctx i = case getBinding ctx i of
    VarBind tyT -> tyT
    _ -> error $
        printf "getTypeFromContext: Wrong kind of binding for variable %s"
               (show $ index2name ctx i)

index2name :: Context -> Int -> String
index2name ctx i = fst $ varLookup ctx i

varLookup :: Context -> Int -> (String, Binding)
varLookup ctx i = atNote
    (printf "Variable lookup failure: offset: %d, ctx size: %d" i (length ctx))
    ctx i

substInTy :: String -> Ty -> Ty -> Ty
substInTy idX tyT tyS = go tyS where go = \case {
    TyArr tyS1 tyS2 -> TyArr (go tyS1) (go tyS2);
    TyNat -> TyNat;
    TyBool -> TyBool;
    TyId s -> if s == idX then tyT else TyId s;
    }

applySubst :: Constr -> Ty -> Ty
applySubst constr tyT = foldl
    (\tyS (TyId idX, tyC2) -> substInTy idX tyC2 tyS)
    tyT (reverse constr)

substInConstr :: String -> Ty -> Constr -> Constr
substInConstr idX tyT constr = flip map constr $ \(tyS1, tyS2) ->
    (substInTy idX tyT tyS1, substInTy idX tyT tyS2)

occursIn :: String -> Ty -> Bool
occursIn idX tyT = go tyT where go = \case {
    TyArr tyT1 tyT2 -> go tyT1 || go tyT2;
    TyNat -> False;
    TyBool -> False;
    TyId s -> s == idX;
    }

-- recon :: Context -> Ty -> State Int (Ty, Constr)
-- recon ctx (TmVar i _) =

unify :: Constr -> Either String Constr
unify [] = Right []
unify ((tyS, TyId idX):rest) =
    if | tyS == TyId idX -> unify rest
       | occursIn idX tyS -> Left "circular constraints"
       | otherwise -> Right ((TyId idX, tyS):) <*> (unify rest)
unify ((TyId idX, tyT):rest) =
    if | tyT == TyId idX -> unify rest
       | occursIn idX tyT -> Left "circular constraints"
       | otherwise -> Right ((TyId idX, tyT):) <*> (unify rest)
unify ((TyBool, TyBool):rest) = unify rest
unify ((TyNat, TyNat):rest) = unify rest
unify ((TyArr tyS1 tyS2, TyArr tyT1 tyT2):rest) = unify $
    (tyS1, tyT1):(tyS2, tyT2):rest
unify ((x, y):_) = Left $ "Could not unify " ++ show x ++ " and " ++ show y

main :: IO ()
main = do
    let (result, constraints) = flip evalState 0 $ recon [("y", VarBind TyBool)] $
          TmApp (TmAbs "x" TyBool TmTrue)
                (TmIf TmFalse (TmVar 0 1) TmTrue)
    print result
    print $ unify constraints

{-
    = TmTrue
    | TmFalse
    | TmIf Term Term Term
    | TmVar Int Int
    | TmAbs String Ty Term
    | TmApp Term Term
-}
