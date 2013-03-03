{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Parse where

import Control.Applicative (Applicative, (<$>), (*>), (<*), (<*>), (<|>))
import qualified Control.Applicative as A
import Control.Monad.State (StateT(StateT), MonadState, runStateT, get, modify, put)
import Control.Monad.Trans (lift)
import Control.Monad.Identity (Identity)
import Control.Monad (liftM)
import qualified Text.Parsec as P
import Data.Char (isLower, isAlpha)
import Data.Monoid ((<>))
import Data.Text as T
import Data.Text.IO as T
import Data.Text.Format as F
import Data.Text.Lazy (toStrict)
import Data.List as L
import Debug.Trace (trace)

import Untyped

data Command
    = Eval Info Term
    | Bind Info Text Binding
    deriving (Show)

printcmd :: Context -> Command -> Text
printcmd ctx (Eval _ tm) = printtm ctx tm
printcmd ctx (Bind _ t _) = t <> "/"

newtype TermParser a = TermParser {
    runTermParser :: StateT Context (P.Parsec Text ()) a
    } deriving (Functor, Applicative, Monad, MonadState Context)

runTP :: TermParser a -> Context -> P.Parsec Text () (a, Context)
runTP = runStateT . runTermParser

instance A.Alternative TermParser where
    a <|> b = TermParser . StateT $ \s -> runTP a s P.<|> runTP b s
    empty = A.empty

parse :: TermParser a -> Context -> Text -> Either P.ParseError (a, Context)
parse tp s1 text =
    let tp' = runStateT (runTermParser tp) s1
    in P.runParser tp' () "parse" text

example :: Text
example = T.unlines [
      "x/;"
    , "x;"
    , "lambda x. x;"
    , "(lambda x. x) (lambda x. x x);"
    ]

example' :: Either P.ParseError ([Command], Context)
example' = parse toplevel [] example

example'' :: IO ()
example'' = do
    let txt = do
        (cmds, ctx) <- example'
        let pnt cmd = case cmd of
                (Eval _ tm) -> printtm ctx $ eval ctx tm
                (Bind _ _ _) -> printcmd ctx cmd
        return $ T.unlines $ L.map pnt cmds
    either Prelude.print T.putStr txt

testParser :: TermParser a -> String -> Either P.ParseError (a, Context)
testParser p s = parse p [] (pack s)

char :: Char -> TermParser Char
char = TermParser . lift . P.char

spaces, eof :: TermParser ()
spaces = TermParser . lift $ P.spaces
eof = TermParser . lift $ P.eof

string :: String -> TermParser String
string = TermParser . lift . P.string

lookAhead :: P.Parsec Text () a -> TermParser a
lookAhead = TermParser . lift . P.lookAhead

anyChar :: TermParser Char
anyChar = TermParser . lift $ P.anyChar

satisfy :: (Char -> Bool) -> TermParser Char
satisfy = TermParser . lift . P.satisfy

try :: TermParser a -> TermParser a
try x = TermParser . StateT $ P.try . runStateT (runTermParser x)

-- | "The top level of a file is a sequence of commands, each terminated
--    by a semicolon." Each command sees the context left by the previous
--    command.
toplevel :: TermParser [Command]
toplevel = done <|> commands where
    done = eof *> return []
    commands = (:) <$> (command <* (char ';' >> spaces)) <*> toplevel

-- | "A top-level command"
command :: TermParser Command
command = cmdBinder <|> cmdTerm where
    cmdTerm = Eval Info <$> term
    cmdBinder = try $ do
        identifier <- lowerIdentifier
        bind <- binder
        modify ((identifier, bind):)
        return $ Bind Info identifier bind

-- | "Right-hand sides of top-level bindings"
binder :: TermParser Binding
binder = char '/' *> return NameBind

term :: TermParser Term
term = abstraction <|> appTerm where
    abstraction = do
        string "lambda" >> spaces
        identifier <- lowerIdentifier
        char '.' >> spaces
        ctx <- get
        tm <- modify ((identifier, NameBind):) >> term
        put ctx
        return $ TmAbs Info identifier tm

-- | Apply one or more lambdas.
appTerm :: TermParser Term
appTerm = do
    tms <- A.many (atomicTerm <* spaces)
    if L.null tms then A.empty else return $ L.foldl1' (TmApp Info) tms

-- | "Atomic terms are ones that never require extra parentheses"
atomicTerm :: TermParser Term
atomicTerm = parenTerm <|> identifierTerm where
    parenTerm = char '(' *> term <* char ')'
    identifierTerm = do
        identifier <- lowerIdentifier
        ctx <- get
        return $ TmVar Info (name2index Info ctx identifier) (ctxlength ctx)

-- | A name beginning with a lower-case letter.
lowerIdentifier :: TermParser Text
lowerIdentifier = do
    lookAhead $ P.try $ P.satisfy isLower
    pack <$> A.many (satisfy isAlpha)
