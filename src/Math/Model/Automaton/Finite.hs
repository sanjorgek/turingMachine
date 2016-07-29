{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE TypeOperators #-}
{-|
Module      : Finite Automaton
Description : Finite Automaton
Copyright   : (c) Jorge Santiago Alvarez Cuadros, 2016
License     : GPL-3
Maintainer  : sanjorgek@ciencias.unam.mx
Stability   : experimental
Portability : portable

Finite Automaton is a stateful machine where all transition means that machine 
reads a symbol
-}
module Math.Model.Automaton.Finite
(
	-- * Recognizer
  -- ** Functions
	Delta(..)
	,DeltaN(..) 
  -- ** Constructor   
	,FiniteA(..)
	,checkString
	-- * Transducer
  -- ** Functions  
	,Lambda1(..)
	,Lambda2(..)
  -- ** Constructor     
	,Transductor(..)
	,translate
  -- * Auxiliar functions
  ,getAlphabet
  ,finalState
  ,finalsStates
  -- ** Create deltas and lambdas
	,liftD
	,liftL1
	,liftL2
	,liftDN
  -- ** Mininmize delta
) where
import Data.State
import Data.Sigma
import Data.Delta
import Data.List
import Data.Monoid
import Control.Monad
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.Foldable as Fold

{-|
Transition function that for every pair, a State and a Symbol by domain, decide next state in machine
-}
type Delta a = (:->:) a Symbol ()

{-|
Lift a list of 3-tuples to a Delta

>>>let delta = liftD [(0,'0',0),(0,'1',1),(1,'0',1),(1,'1',0)]
-}
liftD::(Ord a) => [(a,Symbol,a)] -> Delta a
liftD ds = let
		(xs,ys,zs) = unzip3 ds
		f = map return 
		xys = zip (f xs) ys
		qzs = zip (f zs) (repeat ())
	in Map.fromList (zip xys qzs)

{-|
Transition function that for every pair, a State and a Symbol by domain, decide next list of states in machine
-}
type DeltaN a = (:-<:) a Symbol ()

{-|
Lift a list of 3-tuples to a non deterministic delta

>>>let deltaN = liftDN [(0,'0',[0]),(0,'1',[1]),(1,'0',[1]),(1,'1',[0])]
-}
liftDN::(Ord a) => [(a,Symbol,[a])] -> DeltaN a
liftDN ds = let
		(xs,ys,zs) = unzip3 ds
		f = map return
		xys = zip (f xs) ys
		qzs = zip (map f zs) (repeat ())
	in Map.fromList (zip xys qzs)

{-|
Transducer function
-}
type Lambda1 a = (:*>:) a () Symbol

{-|
Lift simple transducer function
-}
liftL1::(Ord a) => [(a, Symbol)] -> Lambda1 a
liftL1 ds = let
		(xs, ys) = unzip ds
		f = map return
		nds = zip (zip (f xs) (repeat ())) ys
	in Map.fromList nds

{-|
Transducer function with output at transition
-}
type Lambda2 a = (:*>:) a Symbol Symbol

{-|
Lift second transducer function
-}
liftL2::(Ord a) => [(a, Symbol, Symbol)] -> Lambda2 a
liftL2 ds = let
		(xs, ys, zs) = unzip3 ds
		f = map return
		nds = zip (zip (f xs) ys) zs
	in Map.fromList nds
 
{-|
Finite deterministic Automaton
-}
data FiniteA a = 
	-- |>>>let autFin = F delta (Set.fromList [Q 0]) (Q 0)
	F (Delta a) (Final a) (State a)
	-- |>>>let autFinN = FN deltaN (Set.fromList [Q 0]) (Q 0)
	| FN (DeltaN a) (Final a) (State a) deriving(Show,Eq)

{-|
Gets alphabet for some finite automaton
-}
getAlphabet:: FiniteA a -> Alphabet
getAlphabet (F d _ _) = Set.fromList (getFirstParam d)
getAlphabet (FN dn _ _) = Set.fromList (getFirstParam dn)
    
finalState::(Ord a) => Delta a -> State a -> Wd -> State a
finalState _ q [] = q
finalState dt q (x:xs) = finalState dt (nextD dt (q,x)) xs

finalsStates::(Ord a) => DeltaN a -> [State a] -> Wd -> [State a]
finalsStates _ qs [] = qs
finalsStates dn qs (x:xs) = let
    mDelta dt lq a = (nub.concatMap (\q -> nextND dt (q,a))) lq
  in
    finalsStates dn (mDelta dn qs x) xs

{-|
Executes a automaton over a word

>>>checkString autFin "1010010101101010"
True
>>>checkString autFin "1010010101101010001010101010"
False
-}
checkString::(Ord a) => FiniteA a -> Wd -> Bool
checkString (F d qF s) ws = let
		q = finalState d s ws
		f y = (not.isError) y && terminal qF y
	in f q
checkString (FN dn qF s) ws = let
		qs = finalsStates dn [s] ws
		f y = (not.isError) y && terminal qF y
		g = any f
	in g qs

{-|
Transducer Autmaton, both types:

1. Moore

2. Mealy
-}
data Transductor a = 
	Moore (Delta a) (Lambda1 a) (Final a) (State a) 
	|Mealy (Delta a) (Lambda2 a) (Final a) (State a) deriving(Show, Eq)

{-|
For every transducer, given a word the automaton change all symbols in lambda
-}
translate::(Ord a) => Transductor a -> Wd -> Wd
translate (Moore d l qF s) ws = let
		(q, w) = translate' d l s ws []
	in w
	where
		translate' _ _ QE xs ys = (QE, "Error: \nCadena:"++xs++"\nResp parcial: "++ys)
		translate' _ _ q [] xs = (q, xs)
		translate' dt lm q (y:ys) xs = translate' dt lm (nextD dt (q,y)) ys (xs++[lm Map.! (q, ())])
translate (Mealy d l qF s) ws = let
		(q, w) = translate' d l s ws []
	in ws
	where 
		translate' _ _ QE xs ys = (QE, "Error: \nCadena:"++xs++"\nResp parcial: "++ys)
		translate' _ _ q [] xs = (q, xs)
		translate' dt lm q (x:xs) ys = translate' dt lm (nextD dt (q, x)) xs (ys++[lm Map.! (q,x)])
{-
kDistinguishable::FiniteA a -> State a -> State a -> Integer -> Bool,Integer
kDistinguishable 
-}