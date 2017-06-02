module SandException where

import Control.Exception.Safe
import Control.Monad

--------------------------------------------------

data ExampleException = ExampleException String

instance Show ExampleException where
  show (ExampleException s) = concat ["Example Ex: ", s]

instance Exception ExampleException

{-

throw :: (Exception e, MonadThrow m) => e -> m a
try   :: (Exception e, MonadCatch m) => m a -> m (Either e a)
catch :: (Exception e, MonadCatch m) => m a -> (e -> m a) -> m a

Instances of MonadThrow
- []
- Maybe
- IO
- Either
- ExceptT
lc
-}

failure :: (MonadThrow m) => m a
failure = throw $ ExampleException "beep boop"

--------------------------------------------------
-- Failure in IO

failIO :: IO ()
failIO = do
  failure
  putStrLn "I won't get printed :("

failIO2 :: IO ()
failIO2 = do
  a <- (try failure) :: IO (Either ExampleException Int)
  b <- (try failure) :: IO (Either ExampleException Int)
  c <- (try failure) :: IO (Either ExampleException Int)
  putStrLn $ either (\(ExampleException s) -> s) (const "didn't fail") a
  putStrLn $ "but I completed anyway, because the failures were `tried`"

--------------------------------------------------
-- Generic Failure, multiple interpretations

failDiv :: (MonadThrow m, Fractional a, Eq a, Num a) => a -> a ->  m a
failDiv x y
  | y == 0 = failure -- notice, this is able to reuse the original
                     -- `failure` fn
  | otherwise = return $ x / y

{-

λ> failDiv 3 0 :: Either SomeException Float
Left Example Ex: beep boop

λ> failDiv 3 0 :: IO Float
*** Exception: Example Ex: beep boop

λ> failDiv 3 0 :: Maybe Float
Nothing

λ> failDiv 3 0 :: [Float]
[]

λ> failDiv 13 <$> [0,1,2] :: [[Rational]]
[[],[13 % 1],[13 % 2]]

λ> (failDiv <$> [1,2,3]) <*> [1,0] :: [Maybe Float]
[Just 1.0,Nothing,Just 2.0,Nothing,Just 3.0,Nothing]

-}



  
  
