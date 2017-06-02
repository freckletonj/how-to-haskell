{-# LANGUAGE DeriveFoldable #-}

import Data.Foldable

data Foo a = Foo { a :: a
                 , b :: a
                 , c :: a} deriving (Foldable)

f = mapM_ (putStrLn . show . (+1)) (Foo 1 2 3) -- ==> prints lines 2 3 4
