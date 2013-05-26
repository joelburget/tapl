{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- Main entry point for various interpreters from TAPL.
-
- <Talk about command line options>
-
- Present a simple prompt and evaluate one statment at a time:
-
- > x/;
- x/
- > x;
- x
- > lambda x. x;
- (lambda x'. x')
- > (lambda x. x) (lambda x. x x)
- (lambda x'. x' x')
- > lambda y. x
- (lambda y. x)
-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (catch)
import Control.Monad (void)
import Control.Monad.Trans (liftIO)
import Control.Wire
import Data.Text as T
import Data.Text.IO as T
import Data.Time.Clock
import Prelude hiding ((.), id)
import System.IO (hFlush, stdout, isEOF)
import System.Exit

import Stlc

-- Inhibit with a Text error message, run under IO
type MyWire = Wire Text IO

evaluate :: MyWire Text Text
evaluate = mkState [] $ \_ (input, ctx) -> case parse command ctx input of
    Left err -> (Left (T.pack $ show err), ctx)
    Right (cmd, ctx') -> (Right (printcmd ctx' cmd), ctx')

prompt :: MyWire () Text
prompt = prompt' >>> getIt
    where prompt' = perform . pure (T.putStr "> " >> hFlush stdout)
          eof = perform . pure isEOF
          getIt = eof >>> when not >>> perform . pure T.getLine

deltaClockSession :: Int -> Session IO
deltaClockSession dt =
    Session $ do
        t0 <- liftIO getCurrentTime
        return (0, loop t0)
    where loop t' = Session $ do
            threadDelay dt
            t <- liftIO getCurrentTime
            let realdt = realToFrac (diffUTCTime t t')
            return (realdt, loop t)

control :: (Text -> IO Text) -> (Text -> IO Text) -> MyWire () Text -> IO ()
control whenInhibited whenProduced wire = void $ loop wire (deltaClockSession 100)
  where
    loop w' session' = do
        (mx, w, session) <- stepSession w' session' ()
        case mx of
            Left ex -> whenInhibited ex
            Right x -> whenProduced x
        loop w session `catch` (\(_ :: SomeException) -> T.putStrLn "done!" >> return "done!")

main :: IO ()
main = do
    let f x = T.putStrLn x >> return x
        g _ = T.putStrLn "" >> exitSuccess
    control g f $ prompt >>> evaluate
