{-# LANGUAGE ScopedTypeVariables #-}
module Main where

-- http://cgit.gitano.org.uk/youtube/bf.git/tree/bf.hs
-- http://llvm.org/docs/tutorial/OCamlLangImpl3.html
-- http://augustss.blogspot.com/2009/01/llvm-llvm-low-level-virtual-machine-is.html

import Control.Monad (join)
import Data.Int
import Data.Word
import Foreign.Marshal.Array
import LLVM.Core
import LLVM.ExecutionEngine

data Ty
    = TyBool
    | TyArr Ty Ty
    deriving (Eq, Show)

data Term
    = TmTrue
    | TmFalse
    | TmIf Term Term Term
    | TmVar Int Int
    | TmAbs String Ty Term
    | TmApp Term Term
    deriving (Eq, Show)

{-
type Context = [(Text, Binding)]

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
-}

execute :: Term -> IO ()
execute tm = join $ simpleFunction $ createFunction ExternalLinkage (ret $ compile tm)

-- CodeGenFunction r a
-- r is not used / phantom?
-- a is return value

compile :: Term -> CodeGenFunction r (Value Bool)
compile TmTrue = return $ valueOf True
compile TmFalse = return $ valueOf False
compile (TmIf tmC tm1 tm2) = do
    br1 <- newBasicBlock
    br2 <- newBasicBlock

    -- (test :: Value Bool) <- compile tmC :: CodeGenFunction (Value Bool) Terminate
    -- condBr test br1 br2 :: CodeGenFunction () Terminate
    test <- compile tmC
    condBr test br1 br2

    defineBasicBlock br1
    v1 <- compile tm1
    call v1

    defineBasicBlock br2
    v2 <- compile tm2
    call v2
-- compile (TmVar x y)
-- compile (TmAbs str _ tm)
-- compile (TmApp tm1 tm2)

-- f x y z = (x + y) * z
mAddMul :: CodeGenModule (Function (Int32 -> Int32 -> Int32 -> IO Int32))
mAddMul = createFunction ExternalLinkage $ \x y z -> do
    t <- add x y
    r <- mul t z
    ret r

mFib :: CodeGenModule (Function (Word32 -> IO Word32))
mFib = do
    fib <- newFunction ExternalLinkage
    defineFunction fib $ \arg -> do
        recurse <- newBasicBlock
        exit <- newBasicBlock

        -- only move on to recurse if arg > 2
        test <- cmp CmpGT arg (2::Word32)
        condBr test recurse exit

        defineBasicBlock exit
        ret (1::Word32)

        defineBasicBlock recurse
        x1 <- sub arg (1::Word32)
        fibx1 <- call fib x1
        x2 <- sub arg (2::Word32)
        fibx2 <- call fib x2
        r <- add fibx1 fibx2
        ret r
    return fib

bldGreet :: CodeGenModule (Function (IO ()))
bldGreet = do
    puts <- newNamedFunction ExternalLinkage "puts" :: TFunction (Ptr Word8 -> IO Word32)
    withStringNul "Hello, World!" $ \greetz -> createFunction ExternalLinkage $ do
        tmp <- getElementPtr greetz (0::Word32, (0::Word32, ()))
        call puts tmp -- Throw away return value (?)
        ret ()

mAbs :: CodeGenModule (Function (Int32 -> IO Int32))
mAbs = createFunction ExternalLinkage $ \x -> do
    top <- getCurrentBasicBlock
    xneg <- newBasicBlock
    xpos <- newBasicBlock
    t <- cmp CmpLT x (0::Int32)
    condBr t xneg xpos

    defineBasicBlock xneg
    x' <- sub (0::Int32) x
    br xpos

    defineBasicBlock xpos
    r <- phi [(x, top), (x', xneg)]
    r1 <- add r (1::Int32)
    ret r1

mDotProd :: CodeGenModule (Function (Word32 -> Ptr Double -> Ptr Double -> IO Double))
mDotProd = createFunction ExternalLinkage $ \size aPtr bPtr -> do
    top <- getCurrentBasicBlock
    loop <- newBasicBlock
    body <- newBasicBlock
    exit <- newBasicBlock

    br loop

    defineBasicBlock loop
    i <- phi [(valueOf (0 :: Word32), top)]
    s <- phi [(valueOf 0, top)]
    t <- cmp CmpNE i size
    condBr t body exit

    defineBasicBlock body

    ap <- getElementPtr aPtr (i, ())
    bp <- getElementPtr bPtr (i, ())
    a <- load ap
    b <- load bp
    ab <- mul a b
    s' <- add s ab

    i' <- add i (valueOf (1 :: Word32))

    addPhiInputs i [(i', body)]
    addPhiInputs s [(s', body)]
    br loop

    defineBasicBlock exit
    ret (s :: Value Double)

main = do
    initializeNativeTarget

    addMul <- unsafePurify `fmap` simpleFunction mAddMul
    fib <- unsafePurify `fmap` simpleFunction mFib
    greet <- simpleFunction bldGreet
    abs <- unsafePurify `fmap` simpleFunction mAbs
    ioDotProd <- simpleFunction mDotProd
    let dotProd a b =
            unsafePurify $
            withArrayLen a $ \aLen aPtr ->
            withArrayLen b $ \bLen bPtr ->
            ioDotProd (fromIntegral (aLen `min` bLen)) aPtr bPtr

    print $ addMul 2 3 4
    print $ fib 35
    greet
    print $ abs 10
    print $ [1, 2, 3] `dotProd` [4, 5, 6]
