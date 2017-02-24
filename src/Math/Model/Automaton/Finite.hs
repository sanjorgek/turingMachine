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
module Math.Model.Automaton.Finite (
  -- * Recognizer
  -- ** Functions
	Delta(..)
	,NDelta(..)
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
  ,endState
  ,endStates
  -- ** Create deltas and lambdas
	,liftDelta
	,liftL1
	,liftL2
	,liftNDelta
  -- ** Mininmize delta
  ,reachableDelta
  ,distinguishableDelta
  ,minimizeFinite
  -- ** Equivalence
  ,convertFA'
  ,convertFA
) where
import           Data.Delta
import qualified Data.Foldable   as Fold
import           Data.Helper
import           Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import           Data.Sigma
import           Data.State

tupleVoid:: (a,b,c) -> (a,b,c,())
tupleVoid (a,b,c) = (a,b,c,())

{-|
Transition function that for every pair, a State and a Symbol by domain, decide
next state in machine
-}
type Delta a = (:->:) a Symbol ()

{-|
Lift a list of 3-tuples to a Delta

>>>let delta = liftDelta [(0,'0',0),(0,'1',1),(1,'0',1),(1,'1',0)]
-}
liftDelta::(Ord a) => [(a,Symbol,a)] -> Delta a
liftDelta ds = liftD $ map tupleVoid ds

{-|
Transition function that for every pair, a State and a Symbol by domain, decide
next list of states in machine
-}
type NDelta a = (:-<:) a Symbol ()

{-|
Lift a list of 3-tuples to a non deterministic delta

>>>let deltaN = liftNDelta [(0,'0',[0]),(0,'1',[1]),(1,'0',[1]),(1,'1',[0])]
-}
liftNDelta::(Ord a) => [(a,Symbol,[a])] -> NDelta a
liftNDelta ds = liftND $ map tupleVoid ds

{-|
Transducer function
-}
type Lambda1 a = (:*>:) a () Symbol

tupleMidVoid :: (a, b) -> (a, (), b)
tupleMidVoid (a, b) = (a, (), b)

{-|
Lift simple transducer function
-}
liftL1::(Ord a) => [(a, Symbol)] -> Lambda1 a
liftL1 = liftL . map tupleMidVoid

{-|
Transducer function with output at transition
-}
type Lambda2 a = (:*>:) a Symbol Symbol

{-|
Lift second transducer function
-}
liftL2::(Ord a) => [(a, Symbol, Symbol)] -> Lambda2 a
liftL2 = liftL

{-|
Finite deterministic Automaton
-}
data FiniteA a =
	-- |>>>let autFin = F delta (Set.fromList [Q 0]) (Q 0)
	F (Delta a) (Final a) (State a)
	-- |>>>let autFinN = FN deltaN (Set.fromList [Q 0]) (Q 0)
	| FN (NDelta a) (Final a) (State a) deriving(Show,Eq)

{-|
Gets alphabet for some finite automaton
-}
getAlphabet:: FiniteA a -> Alphabet
getAlphabet (F d _ _) = getFirstParamSet d
getAlphabet (FN dn _ _) = getFirstParamSet dn

getAlphabetList::FiniteA a -> [Symbol]
getAlphabetList (F d _ _) = getFirstParam d
getAlphabetList (FN dn _ _) = getFirstParam dn

{-|
For some delta, an initial state anf a word returns final state for that word
-}
endState::(Ord a) => Delta a -> State a -> Wd -> State a
endState _ q [] = q
endState dt q (x:xs) = endState dt (nextD dt (q,x)) xs

{-|
Same as endState but work with no deterministic delta
-}
endStates::(Ord a) => NDelta a -> SetState a -> Wd -> SetState a
endStates _ sq [] = sq
endStates dn sq (x:xs) = let
    nsq = Set.map (\q -> nextND dn (q,x)) sq
  in
    endStates dn ((Set.unions . Set.toList) nsq) xs

{-|
Executes a automaton over a word

>>>checkString autFin "1010010101101010"
True
>>>checkString autFin "1010010101101010001010101010"
False
-}
checkString::(Ord a) => FiniteA a -> Wd -> Bool
checkString (F d qF s) ws = let
		q = endState d s ws
		f y = (not.isError) y && terminal qF y
	in f q
checkString (FN dn qF s) ws = let
		sq = endStates dn (Set.fromList [s]) ws
		qs = Set.toList sq
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
		translate' dt lm q (y:ys) xs = translate' dt lm (nextD dt (q,y)) ys (xs++[nextT lm (q, ())])
translate (Mealy d l qF s) ws = let
		(q, w) = translate' d l s ws []
	in w
	where
		translate' _ _ QE xs ys = (QE, "Error: \nCadena:"++xs++"\nResp parcial: "++ys)
		translate' _ _ q [] xs = (q, xs)
		translate' dt lm q (x:xs) ys = translate' dt lm (nextD dt (q, x)) xs (ys++[nextT lm (q,x)])

reachableStates1 alp d xs = let
    qs = (xs ++ [nextD d (y,x) | x<-alp, y<-xs])\\[QE]
    nqs = nub qs
  in
    if nqs==xs then nqs else reachableStates1 alp d nqs

reachableStates2 alp d xs = let
    qs = (xs ++ concat [Set.toList (nextND d (y,x)) | x<-alp, y<-xs])\\[QE]
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
    alp = getAlphabetList af
    qs = reachableStates1 alp d [si]
    ks = [(x,y) | x<-qs, y<-alp]
    nDelta = foldl (\x k -> Map.insert k (nextD d k,()) x) Map.empty ks
  in
    F nDelta (Set.intersection sf (Set.fromList qs)) si
reachableDelta afn@(FN dn sf si) = let
    allState = getStateDomain dn
    alp = getAlphabetList afn
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

reachState2 alp d q = (Set.toList . Set.unions) [nextND d (q, a) | a<- alp]

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
Delete redundant states and their transitions, if a state is equivalent to
another then is redundant, two state are equivalent if they are
undistinguisahbles.
-}
distinguishableDelta::(Ord a) => FiniteA a -> FiniteA a
distinguishableDelta af@(F d sf si) = let
    allState = getStateDomain d
    alp = getAlphabetList af
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
    alp = getAlphabetList afn
    p0 = fstPartition sf allState
    qss = lDistinguishable2 alp dn p0
    f (ps:pss) e = if e `elem` ps then head ps else f pss e
    f [] _ = QE
    ks = [(x,y) | x<- map head qss, y<-alp]
    nDelta = foldl (\x k -> Map.insert k (Set.map (f qss) (nextND dn k), ()) x) Map.empty ks
  in
    FN nDelta (Set.map (f qss) sf) si

{-|
Minimize a finite automaton,

1. Delete unreachable states and their transitions

2. Delete redundant states
-}
minimizeFinite::(Ord a) => FiniteA a -> FiniteA a
minimizeFinite = distinguishableDelta . reachableDelta

state2Set::(Ord a) => State a -> Set.Set a
state2Set QE = Set.empty
state2Set (Q x) = Set.fromList [x]

setState2Set'::(Ord a) => Set.Set a -> SetState a -> Set.Set a
setState2Set' sa sP = if sP==Set.empty
  then sa
  else let
      p = Set.elemAt 0 sP
    in setState2Set' (Set.union (state2Set p) sa) (Set.delete p sP)

setState2Set::(Ord a) => SetState a -> Set.Set a
setState2Set = setState2Set' Set.empty

nextStateSet::(Ord a) => NDelta a -> StateSS a -> Symbol -> SetState a
nextStateSet nd qsq a = let
    f q = nextND nd (q, a)
    g = Set.map f
    Q sq = fmap g qsq
  in unionsFold sq

updateDeltaBySym::(Ord a) => NDelta a -> StateSS a -> Symbol -> Delta (SetState a) -> Delta (SetState a)
updateDeltaBySym nd qsq a d = let
    k = (qsq, a)
    psp = Q $ nextStateSet nd qsq a
  in Map.insert k (psp, ()) d

updateDeltaByState::(Ord a) => NDelta a -> StateSS a -> Delta (SetState a) -> Delta (SetState a)
updateDeltaByState nd qsq delta = let
    f d a = updateDeltaBySym nd qsq a d
  in Fold.foldl f delta (getFirstParamSet nd)

updateDelta::(Ord a) => NDelta a -> StateSS a -> Delta (SetState a) -> Delta (SetState a)
updateDelta nd qsq d = let
    dDom = getStateDomainSet d
    newD = updateDeltaByState nd qsq d
    newDDom = getStateDomainSet newD
    difDom = Set.difference dDom newDDom
    f delta psp = updateDelta nd psp delta
  in if Set.null difDom
    then newD
    else Fold.foldl f d difDom

convertFA'::(Ord a) => FiniteA a -> FiniteA (SetState a)
convertFA' (FN nd sqf q0) = let
    alp = getFirstParamSet nd
    newQ0 = Q $ Set.fromList [q0]
    newD = updateDelta nd newQ0 Map.empty
    sf = setState2Set sqf
  in F newD Set.empty newQ0

convertFA::(Ord a) => FiniteA a -> FiniteA a
convertFA (F d sqf q0) = let
    f (x, y) = (Set.fromList [x], y)
  in
    FN (fmap f d) sqf q0
convertFA (FN nd sqf q0) = let
  in
    F Map.empty Set.empty q0
