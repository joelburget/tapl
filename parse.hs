{-# LANGUAGE OverloadedStrings #-}
module Parse where

import Control.Applicative as A
import Control.Monad.State
import Data.Attoparsec.Text as P
import Data.Char (isLower, isAlpha, isSpace)
import Data.Text as T
import Data.List as L
import Debug.Trace (trace)

import Untyped

example :: Text
example = T.unlines [
    --   "x/;"
    -- , "x;"
      "lambda x. x;"
    , "(lambda x. x) (lambda x. x x);"
    ]

example' :: Either String ([Command], Context)
example' = do
    s <- parseOnly toplevel "(lambda x. x) (lambda x. x x);"
    -- s <- parseOnly toplevel "lambda x. x;"
    return $ runState s []

testParser :: Parser (InContext a) -> String -> Either String (a, Context)
testParser p s = do
    s <- parseOnly p (pack s)
    return $ runState s []

data Command
    = Eval Info Term
    | Bind Info Text Binding
    deriving (Show)

type InContext = State Context

-- | Used here as a -> Parser (Incontext a)
return2 :: (Monad m, Monad n) => a -> m (n a)
return2 = return . return

-- | "The top level of a file is a sequence of commands, each terminated
--    by a semicolon."
toplevel :: Parser (InContext [Command])
toplevel = done <|> commands where
    done = endOfInput *> return2 []
    commands = do
        cmd <- trace "toplevel command" $ command <* char ';'
        skipSpace
        trace "skipped space" $ return ()
        cmds <- trace "toplevel commands" toplevel
        return $ (:) <$> cmd <*> cmds

-- | "A top-level command"
command :: Parser (InContext Command)
command = cmdTerm <|> cmdBinder where
    cmdTerm = term >>= \tm -> trace "eval" $ return $ Eval Info <$> tm
    cmdBinder = do
        identifier <- trace "command lowerIdentifier" lowerIdentifier
        bind <- trace "command binder" binder
        return $ Bind Info identifier <$> bind

-- | "Right-hand sides of top-level bindings"
binder :: Parser (InContext Binding)
binder = char '/' *> return2 NameBind

term :: Parser (InContext Term)
-- term = abstraction <|> appTerm where
term = appTerm <|> abstraction where
    -- abstraction = abstraction' <|> (char '(' *> abstraction' <* char ')')
    abstraction = do
        trace "lambda" $ string "lambda"
        skipSpace
        identifier <- lowerIdentifier
        trace (show identifier ++ ".") $ char '.'
        skipSpace
        tm <- term
        trace "got term" $ return ()
        return $ TmAbs Info identifier <$> tm

-- | Apply one or more lambdas.
appTerm :: Parser (InContext Term)
-- appTerm = appTerm' <|> atomicTerm where 
--     appTerm' = do
--         tm <- atomicTerm
--         applied <- appTerm
--         return $ TmApp Info <$> applied <*> tm
appTerm = do
    tms <- many' atomicTerm
    trace ("appTerm: " ++ show (L.length tms)) $ return ()
    if L.null tms then A.empty else
        return $ L.foldl1' (\tm1 tm2 -> TmApp Info <$> tm1 <*> tm2) tms

-- | "Atomic terms are ones that never require extra parentheses"
atomicTerm :: Parser (InContext Term)
atomicTerm = parenTerm <|> identifierTerm where
    parenTerm = char '(' *> term <* char ')'
    identifierTerm = do
        identifier <- lowerIdentifier
        return $ do
            ctx <- get
            return $ TmVar Info (name2index Info ctx identifier) (ctxlength ctx)

-- | A name beginning with a lower-case letter.
lowerIdentifier :: Parser Text
-- lowerIdentifier = skip isLower *> takeWhile1 isAlpha
lowerIdentifier = do
    c <- trace "lowerIdentifier peekChar" peekChar
    guard (maybe False isLower c)
    takeWhile1 isAlpha
