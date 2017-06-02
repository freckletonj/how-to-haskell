{-# LANGUAGE FlexibleContexts #-}
module Transformer where

import Control.Monad
import Text.Read (readMaybe)
-- import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
-- import Control.Monad.Trans.Except

-- import Control.Monad.Trans.Writer -- transformers
import Control.Monad.Writer -- mtl
import Control.Monad.Reader

import Control.Monad.Except



problem :: MonadWriter [String] m => m () -- Monad m => WriterT [String] m ()
problem = do
  tell ["a"]
  tell ["b"]
  fail "oops"

type A = WriterT [String] Maybe
type B = MaybeT (Writer [String])

a = runWriterT (problem :: A ()) -- ==> Nothing
b = runWriter . runMaybeT $ (problem :: B ()) -- ==> (Nothing, ["a", "b"])
