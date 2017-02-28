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
data Discrete = Fin Integer | Numerable deriving(Show, Eq)

instance Ord Discrete where
  compare (Fin n) (Fin m) = compare n m
  compare Numerable Numerable = EQ
  compare Numerable _ = GT
  compare _ _ = LT

instance Bounded Discrete where
  minBound = Fin 0
  maxBound = Numerable

instance Monoid (Sum Discrete) where
  mempty = Sum $ Fin 0
  mappend (Sum (Fin n)) (Sum (Fin m)) = Sum $ Fin $ getSum $ mappend (Sum n) (Sum m)
  mappend (Sum Numerable) _ = Sum Numerable
  mappend _ _ = Sum Numerable

instance Monoid (Product Discrete) where
  mempty = Product $ Fin 1
  mappend (Product (Fin 0)) _ = Product $ Fin 0
  mappend _ (Product (Fin 0)) = Product $ Fin 0
  mappend (Product (Fin n)) (Product (Fin m)) = Product $ Fin $ getProduct $ mappend (Product n) (Product m)
  mappend (Product Numerable) _ = Product Numerable
  mappend _ (Product Numerable) = Product Numerable
