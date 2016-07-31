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
  ,DeltaN(..)
	,liftD
	,StackA(..)
  ,getInputAlphabet
  ,getStackAlphabet
	,checkString
) where
import Data.List
import Data.Monoid
import qualified Data.Map.Strict as Map
import qualified Data.Foldable as Fold
import Data.Delta
import Data.State
import Data.Sigma

{-|
Delta for stack machine, takes a state, a symbol in string input and a symbol in
stack head and returns next state and update stack
-}
type Delta a = (:->:) a (Symbol, Symbol) [Symbol]

{-|
Delta for stack machine, takes a state, a symbol in string input or not and a 
symbol in stack head and returns next state and update stack
-}
type DeltaN a = (:-<:) a (Either Symbol Epsilon, Symbol) [Symbol]

{-|
Takes a list of tuples and lift a Delta

>>>let delta = liftD [(0,'1','A',1,[AA]),(0,'0',blank,0,[A])]
-}
liftD::(Ord a) => [(a, Symbol, Symbol, a, [Symbol])]-> Delta a
liftD xs = let
		(as,bs,cs,ds,es) = unzip5 xs
		f = map Q
		p = zip bs cs
		k = zip (f as) p
		r = zip (f ds) es
	in Map.fromList (zip k r)

-- |Stack machine only needs a delta, an init state and an initial symbol
data StackA a = 
  Stack (Delta a) (State a) Symbol
  |StackN (DeltaN a) (State a) Symbol 
  
getSigma [] = []
getSigma ((Left x):xs) = x:(getSigma xs)
getSigma (_:xs) = getSigma xs

getInputAlphabet :: StackA t -> [Symbol]
getInputAlphabet (Stack d _ _) = (fst . unzip . getFirstParam) d
getInputAlphabet (StackN dn _ _) = (getSigma . fst . unzip . getFirstParam) dn

getStackAlphabet :: StackA t -> [Symbol]
getStackAlphabet (Stack d _ _) = (snd . unzip . getFirstParam) d
getStackAlphabet (StackN dn _ _) = (snd . unzip . getFirstParam) dn

{-|
Executes a stack machine over a word

>>>checkString autStack 'aaabbbcccccc'
True
-}
checkString::(Ord a) => StackA a -> Wd -> Bool
checkString (Stack d s z0) ws = let
		q = checkString' d s [z0] ws
		f = not.isError
	in f q
	where
		check dt s = if Map.member s dt then dt Map.! s else (QE,[])
		checkString' _ QE _ _ = QE
		checkString' _ q [] [] = q
		checkString' _ _ (_:_) [] = QE
		checkString' _ q [] (_:_) = QE
		checkString' dt q (x:xs) (y:ys) = let 
				(qn, st) = check dt (q, (y, x))
			in checkString' dt qn (st++xs) ys