{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE FlexibleInstances #-}
{-|
Module      : Cardinal
Description : Cardinal Def
Copyright   : (c) Jorge Santiago Alvarez Cuadros, 2016
License     : GPL-3
Maintainer  : sanjorgek@ciencias.unam.mx
Stability   : experimental
Portability : portable

Cardinal def
-}
module Data.Cardinal where
import           Data.Monoid

data Essence = Empty | Occupied deriving(Show, Eq, Ord)

{-|
Cardinal def:

1. Finite Natural value

2. Infinite value
-}
data Discrete = Fin | Numerable deriving(Show, Eq)

instance Bounded Discrete where
  minBound = Fin
  maxBound = Numerable
