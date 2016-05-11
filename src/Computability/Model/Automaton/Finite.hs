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
module Computability.Model.Automaton.Finite
(
	-- * Deterministic
	-- ** Function 
	-- *** Recognizer
	Delta(..)
	,liftD
	-- ** Transducer
	,Lambda1(..)
	,liftL1
	,Lambda2(..)
	,liftL2
	-- ** Constructor
	,FiniteA(..)
	,Transductor(..)
	-- ** Function
	,checkString
	,translate
	-- * Not deterministic
	-- ** Function
	,DeltaN(..)
	,liftDN
	-- ** Constructor
	,FiniteAN(..)
	,checkStringN
) where
import Data.State
import Data.Sigma
import Data.Delta
import Data.List
import Data.Monoid
import Control.Monad
import qualified Data.Map.Lazy as Map
import qualified Data.Foldable as Fold

{-|
Transition function hava a State and a Symbol by domain to decide next state in 
machine
-}
type Delta a = (:->:) a Symbol ()

{-|
Lift a list of 3-tuples in a Delta

>>>let delta = liftD [(0,'0',0),(0,'1',1),(1,'0',1),(1,'1',0)]
-}
liftD::(Ord a) => [(a,Symbol,a)] -> Delta a
liftD ds = let
		(xs,ys,zs) = unzip3 ds
		f = map return 
		xys = zip (f xs) ys
		qzs = zip (f zs) (repeat ())
	in Map.fromList (zip xys qzs)

type Lambda1 a = (:*>:) a () Symbol

liftL1::(Ord a) => [(a, Symbol)] -> Lambda1 a
liftL1 ds = let
		(xs, ys) = unzip ds
		f = map return
		nds = zip (zip (f xs) (repeat ())) ys
	in Map.fromList nds

type Lambda2 a = (:*>:) a Symbol Symbol

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
	-- |>>>let autFin = F delta [Q 0] (Q 0)
	F (Delta a) (Final a) (State a) deriving(Show, Eq)

{-|
Executes a automaton over a word

>>>checkString autFin "1010010101101010"
True
>>>checkString autFin "1010010101101010001010101010"
False
-}
checkString::(Ord a) => FiniteA a -> Wd -> Bool
checkString (F d qF s) ws = let
		q = checkString' d s ws
		f y = ((not.isError) y)&&(terminal qF y)
	in f q
	where
		checkString' _ q [] = q
		checkString' dt q (x:xs) = checkString' dt (nextD dt (q,x)) xs

data Transductor a = 
	Moore (Delta a) (Lambda1 a) (Final a) (State a) 
	|Mealy (Delta a) (Lambda2 a) (Final a) (State a) deriving(Show, Eq)

translate::(Ord a) => Transductor a -> Wd -> Wd
translate (Moore d l qF s) ws = let
		(q, w) = translate d l s ws []
	in w
	where
		translate _ _ QE xs ys = (QE, "Error: \nCadena:"++xs++"\nResp parcial: "++ys)
		translate _ _ q [] xs = (q, xs)
		translate dt lm q (y:ys) xs = translate dt lm (nextD dt (q,y)) ys (xs++[lm Map.! (q, ())])
translate (Mealy d l qF s) ws = let
		(q, w) = translate d l s ws []
	in ws
	where 
		translate _ _ QE xs ys = (QE, "Error: \nCadena:"++xs++"\nResp parcial: "++ys)
		translate _ _ q [] xs = (q, xs)
		translate dt lm q (x:xs) ys = translate dt lm (nextD dt (q, x)) xs (ys++[lm Map.! (q,x)])


type DeltaN a = (:>-:) a Symbol ()

{-|
Lift a list of 3-tuples in a non deterministic delta

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
Finite non deterministic Automaton
-}
data FiniteAN a = 
	-- |>>>let autFinN = FN deltaN (terminal [Q 0]) (Q 0)
	FN (DeltaN a) (Final a) (State a) deriving(Show,Eq)

{-|
Executes a non-deterministic automaton over a word, maybe overload your pc 
-}
checkStringN::(Ord a) => FiniteAN a -> Wd -> Bool
checkStringN (FN dn qF s) ws = let
		qs = checkStringN' dn [s] ws
		f y = ((not.isError) y)&&(terminal qF y)
		g y = or (map f y)
	in g qs
	where
		check dt k = if Map.member k dt then dt Map.! k else ([QE], ())
		mDelta dt lq a = (nub.concat.(map fst)) (map (\q -> check dt (q,a)) lq)
		checkStringN' _ qs [] = qs
		checkStringN' dn qs (x:xs) = checkStringN' dn (mDelta dn qs x) xs