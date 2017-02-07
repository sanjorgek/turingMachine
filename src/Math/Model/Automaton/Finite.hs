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
{-| (
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
  ,finalState
  ,finalsStates
  -- ** Create deltas and lambdas
	,liftDelta
	,liftL1
	,liftL2
	,liftNDelta
  -- ** Mininmize delta
  ,reachableDelta
  ,distinguishableDelta
  ,minimizeFinite
)
-}
where
import           Data.Delta
import qualified Data.Foldable   as Fold
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
finalState::(Ord a) => Delta a -> State a -> Wd -> State a
finalState _ q [] = q
finalState dt q (x:xs) = finalState dt (nextD dt (q,x)) xs

{-|
Same as finalState but work with no deterministic delta
-}
finalsStates::(Ord a) => NDelta a -> Set.Set (State a) -> Wd -> Set.Set (State a)
finalsStates _ sq [] = sq
finalsStates dn sq (x:xs) = let
    nsq = Set.map (\q -> nextND dn (q,x)) sq
  in
    finalsStates dn ((Set.unions . Set.toList) nsq) xs

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
		sq = finalsStates dn (Set.fromList [s]) ws
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
    qs = xs ++ [nextD d (y,x) | x<-alp, y<-xs]
    nqs = (\\) (nub qs) [QE]
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
reachableDelta af@(F d sqf qi) = let
    alp = getAlphabetList af
    qs = reachableStates1 alp d [qi]
    allUnused = (\\) (getStateDomain d) qs
    ks = [(x,y) | x<-allUnused, y<-alp]
    nDelta = foldl (flip Map.delete) d ks
  in
    F nDelta (Set.intersection sqf (Set.fromList qs)) qi
reachableDelta afn@(FN dn sqf qi) = let
    alp = getAlphabetList afn
    qs = reachableStates2 alp dn [qi]
    allUnused = (\\) (getStateDomain dn) qs
    ks = [(x,y) | x<-allUnused, y<-alp]
    nDelta = foldl (flip Map.delete) dn ks
  in
    FN nDelta (Set.intersection sqf (Set.fromList qs)) qi

fstPartitionSet sf qs = let
    (xs,ys) = Set.partition (terminal sf) qs
  in
    Set.delete Set.empty $ Set.fromList [xs, ys]

partitionSet q = Set.filter (Set.member q)
partitionSet2 q = Set.filter (Set.isSubsetOf q)

distinguishableSet alp d partSet pi = let
    qM = Set.findMin pi
    eqD p q = (==) (partitionSet p partSet) (partitionSet q partSet)
    g p q a = eqD (nextD d (p, a)) (nextD d (q, a))
    f p q = Fold.all (g p q) alp
    (sx, sy) = Set.partition (f qM) pi
  in Set.delete Set.empty $ Set.fromList [sx, sy]

distinguishableSet2 alp nd partSet pi = let
    qM = Set.findMin pi
    eqD p q = (==) (partitionSet2 p partSet) (partitionSet2 q partSet)
    g p q a = eqD (nextND nd (p, a)) (nextND nd (q, a))
    f p q = Fold.all (g p q) alp
    (sx, sy) = Set.partition (f qM) pi
  in Set.delete Set.empty $ Set.fromList [sx, sy]

unionsFold:: (Ord a, Fold.Foldable t) => t (Set.Set a) -> Set.Set a
unionsFold = Fold.foldr Set.union Set.empty

lDistinguishableSet alp d partSet = let
    g = distinguishableSet alp d partSet
    f = unionsFold . Set.map g
    nPartSet = f partSet
  in if nPartSet == partSet
    then nPartSet
    else lDistinguishableSet alp d nPartSet

lDistinguishableSet2 alp nd partSet = let
    g = distinguishableSet2 alp nd partSet
    f = unionsFold . Set.map g
    nPartSet = f partSet
  in if nPartSet == partSet
    then nPartSet
    else lDistinguishableSet2 alp nd nPartSet

{-|
Delete redundant states and their transitions, if a state is equivalent to
another then is redundant, two state are equivalent if they are
undistinguisahbles.
-}
distinguishableDelta::(Ord a) => FiniteA a -> FiniteA a
distinguishableDelta af@(F d sf si) = let
    allStateSet = Set.unions [getStateRangeSet d, getStateDomainSet d, sf, Set.fromList [si]]
    pInitSet = fstPartitionSet sf allStateSet
    alp = getAlphabet af
    partSet = lDistinguishableSet alp d pInitSet
    f q = (Set.findMin . Set.findMin) $ partitionSet q partSet
    allNewStateSet = Set.map f allStateSet
    g q delta a = let
        k = (q, a)
        nQ = nextD d k
      in if nQ==QE
        then delta
        else Map.insert k (f nQ, ()) delta
    h delta q = Fold.foldl (g q) delta alp
    newDelta = Fold.foldl h Map.empty allNewStateSet
  in
    F newDelta (Set.map f sf) (f si)
distinguishableDelta afn@(FN nd sf si) = let
    allStateSet = Set.unions [getStateRangeSetND nd, getStateDomainSet nd, sf, Set.fromList [si]]
    pInitSet = fstPartitionSet sf allStateSet
    alp = getAlphabet afn
    partSet = lDistinguishableSet2 alp nd pInitSet
    f q = (Set.findMin . Set.findMin) $ partitionSet q partSet
    allNewStateSet = Set.map f allStateSet
    g q ndelta a = let
        k = (q, a)
        nQ = nextND nd k
      in if Set.null nQ
        then ndelta
        else Map.insert k (Set.map f nQ, ()) ndelta
    h ndelta q = Fold.foldl (g q) ndelta alp
    newDelta = Fold.foldl h Map.empty allNewStateSet
  in
    afn

{-|
Minimize a finite automaton,

1. Delete redundant states

2. Delete unreachable states and their transitions
-}
minimizeFinite::(Ord a) => FiniteA a -> FiniteA a
minimizeFinite = reachableDelta . distinguishableDelta

state2Set::(Ord a) => State a -> Set.Set a
state2Set QE = Set.empty
state2Set (Q x) = Set.fromList [x]

setState2Set'::(Ord a) => Set.Set a -> Set.Set (State a) -> Set.Set a
setState2Set' sa sP = if sP==Set.empty
  then sa
  else let
      p = Set.elemAt 0 sP
    in setState2Set' (Set.union (state2Set p) sa) (Set.delete p sP)

setState2Set::(Ord a) => Set.Set (State a) -> Set.Set a
setState2Set = setState2Set' Set.empty

nextStateSet::(Ord a) => NDelta a -> State a -> Symbol -> Set.Set a
nextStateSet nd q a = setState2Set $ nextND nd (q, a)

convertFA::(Ord a) => FiniteA a -> FiniteA a
convertFA (F d sqf q0) = let
    f (x, y) = (Set.fromList [x], y)
  in
    FN (fmap f d) sqf q0
convertFA (FN nd sqf q0) = let
  in
    F Map.empty sqf q0
