{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE TypeOperators #-}
{-|
Module      : StackA
Description : Stack Automaton
Copyright   : (c) Jorge Santiago Alvarez Cuadros, 2016
License     : GPL-3
Maintainer  : sanjorgek@ciencias.unam.mx
Stability   : experimental
Portability : portable

Stack Automaton
-}
module Math.Model.Automaton.Stack
(
  Delta(..)
  ,liftD
	,StackA(..)
  ,getInputAlphabet
  ,getStackAlphabet
) where
import Data.List
import Data.Monoid
import qualified Data.Map.Strict as Map
import qualified Data.Foldable as Fold
import Data.Delta
import Data.State
import Data.Sigma

{-|
Delta for stack machine, takes a state, a symbol in string input or not and a 
symbol in stack head and returns next state and update stack
-}
type Delta a = (:->:) a (Either Symbol Epsilon, Symbol) [Symbol]

{-|
Takes a list of tuples and lift a Delta

>>>let delta = liftD [(0,"1",'A',1,[AA]),(0,"",'Z',0,[A])]
-}
liftD::(Ord a) => [(a, [Symbol], Symbol, a, [Symbol])]-> Delta a
liftD xs = let
    (as,bs,cs,ds,es) = unzip5 xs
    f = map Q
    g [] = Right Epsilon
    g (x:_) = Left x
    ps = zip (map g bs) cs
    ks = zip (f as) ps
    rs = zip (f ds) es
  in Map.fromList (zip ks rs)

-- |Stack machine only needs a delta, an init state and an initial symbol
data StackA a = Stack (Delta a) (State a) Symbol
  
getSigma [] = []
getSigma ((Left x):xs) = x:(getSigma xs)
getSigma (_:xs) = getSigma xs

getInputAlphabet :: StackA t -> [Symbol]
getInputAlphabet (Stack dn _ _) = (getSigma . fst . unzip . getFirstParam) dn

getStackAlphabet :: StackA t -> [Symbol]
getStackAlphabet (Stack dn _ _) = (snd . unzip . getFirstParam) dn
