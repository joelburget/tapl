{-# LANGUAGE Arrows #-}

import Control.Wire
import Debug.Trace

type MyWire = Wire () IO

identity :: Wire e m a a
identity = mkPure $ \_ x -> (Right x, identity)

{-
system :: MyWire a String
system = proc _ -> do
    c1 <- countFrom 10 -< 1
    c2 <- countFrom 20 -< 1
    -- identity -< (show (c1 :: Int, c2 :: Int) ++ "\n")
    returnA -< (show (c1 :: Int, c2 :: Int) ++ "\n")
-}

{-
system :: MyWire a String
system = arr (\_ -> 0::Int) >>> (countFrom 10 &&& countFrom 20) >>> arr show
-}

{-
system :: MyWire Int String
system = arr (const 2::Int->Int) >>> countFrom 10 &&& countFrom 20 >>> arr show
-}

test :: MyWire Int b -> Int -> IO (Either () b)
test wire steps = loop wire steps >>= (\(x, w) -> return x) where
    loop wire steps = stepWire wire 1000 1 >>= if steps == 1
        then return
        else (\(_, wire') -> loop wire' (steps-1))

-- {-
system :: MyWire a String
system = proc _ -> do
    c1 <- countFrom 10 -< 1
    if even c1
      then returnA -< "We don't want even c1\n"
      else do
          c2 <- countFrom 20 -< 1
          -- identity -< (show (c1 :: Int, c2 :: Int) ++ "\n")
          returnA -< (show (c1 :: Int, c2 :: Int) ++ "\n")
-- -}

run :: IO ()
run = testWire 1 1000 getLine clockSession system

run2 :: IO (Either () String, MyWire () String, Session IO)
run2 = stepSession system clockSession ()

run3 :: IO (Either () String, MyWire () String)
run3 = stepWire system 1000 ()

run4 :: IO ()
run4 = loop system clockSession where
    loop w' session' = do
        (mx, w, session) <- stepSession w' session' ()
        case mx of
            Left ex -> return ex
            Right x -> putStrLn $ show x
        loop w session
