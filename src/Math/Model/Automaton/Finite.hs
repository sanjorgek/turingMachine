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
  ,reachableDelta
  ,distinguishableDelta
  ,minimizeFinite
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

{-|
For some delta, an initial state anf a word returns final state for that word
-}
finalState::(Ord a) => Delta a -> State a -> Wd -> State a
finalState _ q [] = q
finalState dt q (x:xs) = finalState dt (nextD dt (q,x)) xs

{-|
Same as finalState but work with no deterministic delta
-}
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
	in w
	where 
		translate' _ _ QE xs ys = (QE, "Error: \nCadena:"++xs++"\nResp parcial: "++ys)
		translate' _ _ q [] xs = (q, xs)
		translate' dt lm q (x:xs) ys = translate' dt lm (nextD dt (q, x)) xs (ys++[lm Map.! (q,x)])

reachableStates1 alp d xs = let
    qs = (xs ++ [nextD d (y,x) | x<-alp, y<-xs])\\[QE]
    nqs = nub qs
  in
    if nqs==xs then nqs else reachableStates1 alp d nqs

reachableStates2 alp d xs = let
    qs = (xs ++ concat [nextND d (y,x) | x<-alp, y<-xs])\\[QE]
    nqs = nub qs
  in
    if nqs==xs then nqs else reachableStates2 alp d nqs 

{-|
Minimaize a delta for some finite automaton.
Gets a delta with all reachable states from initial state.
-}
reachableDelta::(Ord a) => FiniteA a -> FiniteA a
reachableDelta af@(F d sf si) = let
    allState = getStateDomain d  
    alp = (Set.toList . getAlphabet) af
    qs = reachableStates1 alp d [si]
    ks = [(x,y) | x<-qs, y<-alp]
    nDelta = foldl (\x k -> Map.insert k (nextD d k,()) x) Map.empty ks
  in
    F nDelta (Set.intersection sf (Set.fromList qs)) si
reachableDelta afn@(FN dn sf si) = let
    allState = getStateDomain dn  
    alp = (Set.toList . getAlphabet) afn
    qs = reachableStates2 alp dn [si]
    ks = [(x,y) | x<-qs, y<-alp]
    nDelta = foldl (\x k -> Map.insert k (nextND dn k,()) x) Map.empty ks
  in
    FN nDelta (Set.intersection sf (Set.fromList qs)) si
    
fstPartition sf qs = let
    (xs,ys) = partition (terminal sf) qs
  in
    nub [xs, ys] \\ [[]]

samePartition [] _ _ = False
samePartition (x:xs) q1 q2
  |elem q1 x && elem q2 x = True
  |otherwise = samePartition xs q1 q2

reachState alp d q = [nextD d (q, a) | a<- alp]

reachState2 alp d q = (nub .concat) [nextND d (q, a) | a<- alp]

distinguishable alp d pss ps@(q:qs) = let
    nqs = reachState alp d q
    f = zipWith (samePartition pss)
    g x = f (reachState alp d x) nqs
    (xs,ys) = partition (and . g) ps
  in
    nub [xs, ys] \\ [[]]
    
distinguishable2 alp d pss ps@(q:qs) = let
    nqs = reachState2 alp d q
    f = zipWith (samePartition pss)
    g x = f (reachState2 alp d x) nqs
    (xs,ys) = partition (and . g) ps
  in
    nub [xs, ys] \\ [[]]
    
lDistinguishable alp d pss = let
    g = distinguishable alp d pss
    f = (nub . concatMap g)
    npss = f pss
  in if npss == pss then pss else lDistinguishable alp d npss
    
lDistinguishable2 alp d pss = let
    g = distinguishable2 alp d pss
    f = (nub . concatMap g)
    npss = f pss
  in if npss == pss then pss else lDistinguishable2 alp d npss

{-|
Delete redundant states and their transitions, if a state is equivalent to another then is redundant, two state are equivalent if they are undistinguisahbles.
-}
distinguishableDelta::(Ord a) => FiniteA a -> FiniteA a
distinguishableDelta af@(F d sf si) = let
    allState = getStateDomain d
    alp = (Set.toList . getAlphabet) af    
    p0 = fstPartition sf allState
    qss = lDistinguishable alp d p0
    f (ps:pss) e = if e `elem` ps then head ps else f pss e
    f [] _ = QE
    ks = [(x,y) | x<- map head qss, y<-alp]
    nDelta = foldl (\x k -> Map.insert k (f qss (nextD d k), ()) x) Map.empty ks
  in
    F nDelta (Set.map (f qss) sf) si
distinguishableDelta afn@(FN dn sf si) = let
    allState = getStateDomain dn
    alp = (Set.toList . getAlphabet) afn
    p0 = fstPartition sf allState
    qss = lDistinguishable2 alp dn p0
    f (ps:pss) e = if e `elem` ps then head ps else f pss e
    f [] _ = QE
    ks = [(x,y) | x<- map head qss, y<-alp]
    nDelta = foldl (\x k -> Map.insert k (map (f qss) (nextND dn k), ()) x) Map.empty ks
  in
    FN nDelta (Set.map (f qss) sf) si

{-|
Minimize a finite automaton,

1. Delete unreachable states and their transitions

2. Delete redundant states
-}
minimizeFinite::(Ord a) => FiniteA a -> FiniteA a
minimizeFinite = distinguishableDelta . reachableDelta
