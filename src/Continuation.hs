{- stack
--resolver lts-8.2
--install-ghc
exec ghci
--package mtl
-}

import           Control.Monad.Cont



calculateLength :: [a] -> Cont r Int
calculateLength l = return (length l)

ex1 = do
  runCont (calculateLength [1..10]) print

----------

whatsYourName :: String -> String
whatsYourName name =
  (`runCont` id) $ do                      -- 1
    response <- callCC $ \exit -> do       -- 2
      validateName name exit               -- 3
      return $ "Welcome, " ++ name ++ "!"  -- 4
    return response                        -- 5

validateName name exit = do
  when (null name) (exit "You forgot to tell me your name!")


----------

-- Call with Current Continuation
-- callCC :: MonadCont m => ((a -> m b) -> m a) -> m a
-- return :: a -> Cont effect a

isEven :: Int -> Bool
isEven x = do
  (`runCont` id) $ do
    response <- callCC $ \exit -> do
      if even x
        then exit True
        else exit False
    return response


-- findABrokenLink :: String -> IO String

countUp :: IO ()
countUp = flip runContT return $ do
    lift $ putStrLn "alpha"
    (k, num) <- callCC $ \k -> let f x = k (f, x)
                               in return (f, 0)
    lift .putStrLn . show $ (num)
    if num < 5
        then k (num + 1) >> return ()
        else lift $ print num
----------

{-
-- https://stackoverflow.com/a/29175418/3884713

-- callCC tends to mean "jump back to this execution context"
-}

data Mu t = In { out :: t (Mu t) }

newtype C' b a = C' { unC' :: a -> b }
type C b = Mu (C' b)

unfold = unC' . out
fold = In . C'

setjmp = callCC $ (\c -> return $ fold c)
jump l = unfold l l

test :: ContT () IO ()
test = do
    lift $ putStrLn "Start"
    l <- setjmp
    lift $ putStrLn "x"
    jump l

-- main = runContT test return -- infinite loop


----------

-- half or triple plus one
-- https://en.wikipedia.org/wiki/3x_%2B_1_problem
hotpo :: Int -> IO ()
hotpo start = flip runContT return $ do
  lift $ putStrLn "Begin"
  (k, state, count) <-
    callCC $ \k -> let f x c = k (f, x, c)
                   in return (f, start, 0)
  lift . putStrLn . show $ state
  if state==1 then lift . putStrLn
                   $ "done after " ++ show count ++ " iterations"
    else if even state then k (div state 2) (count + 1)
    else k (3 * state + 1) (count + 1)
