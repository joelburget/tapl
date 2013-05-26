{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
module Main where

import Control.Applicative ((<*>))
-- import Control.Monad.State

data Ty
    = TyBool
    | TyArr Ty Ty
    | TyId String
    | TyNat
    deriving (Show, Eq)

type Constr = [(Ty, Ty)]

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
main = print $ unify [(TyArr TyNat (TyId "x"), TyArr (TyId "y") TyBool)]
