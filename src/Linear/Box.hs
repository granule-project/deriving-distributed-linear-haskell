{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE RankNTypes #-}
module Linear.Box where
import Prelude.Linear as Linear
import Control.Monad
import Data.Traversable
import Data.Foldable

data Box r a where { Box :: a %r -> Box r a }
  deriving (Show, Functor, Foldable, Traversable)

class Pushable f where
  push :: Box r (f a) %1-> f (Box r a)
