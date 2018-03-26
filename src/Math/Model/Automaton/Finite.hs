{-# OPTIONS_GHC -fno-warn-tabs      #-}
{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE TypeOperators          #-}
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
  -- ** Deltas
	Delta(..)
	,NDelta(..)
	-- *** Create deltas
	,liftDelta
	,liftNDelta
  -- ** Constructor
	,FiniteA(..)
	-- ** Functions
	,getAlphabet
	,checkString
	-- *** Auxiliar functions
	,nextState
	,nextStates
	-- ** Mininmize delta
	,reachableStates'
	,reachableFinite
	,distinguishableFinite
	,minimizeFinite
	-- ** Language
  ,automatonEssence
  ,automatonCardinality
	-- * Transducer
  -- ** Lambdas
	,Lambda1(..)
	,Lambda2(..)
	-- *** Create lambdas
	,liftL1
	,liftL2
  -- ** Constructor
	,Transductor(..)
	-- ** Functions
	,getOutputAlphabet
	,translate
	-- *** Auxiliar functions
	,transMoore
	,transMealy
  -- * Equivalences
	,convertFA
  ,convertTA
	,transducerToFinite
	,finiteToMoore
	,finiteToMealy
) where
import           Data.Numerable
import           Data.Delta
import qualified Data.Foldable   as Fold
import           Data.Label
import           Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import           Data.Sigma
import           Control.Monad.State.Lazy

tupleAUX:: (a,b,c) -> (a,b,Label c)
tupleAUX (a,b,c) = (a,b,Q c)

{-|
Union of set monad
-}
unionsFold:: (Ord a, Fold.Foldable t) => t (Set.Set a) -> Set.Set a
unionsFold = Fold.foldr Set.union Set.empty

{-|
Size of a set, with large integers
-}
setGenericSize:: (Ord a) => Set.Set a -> Integer
setGenericSize s = if Set.null s
  then 0
  else 1 + setGenericSize (Set.delete (Set.findMin s) s)

{-|
Transition function that for every pair, a State and a Symbol by domain, decide
next state in machine
-}
type Delta a = (:*>:) a Symbol (Label a)

{-|
Lift a list of 3-tuples to a Delta

>>>let delta = liftDelta [(0,'0',0),(0,'1',1),(1,'0',1),(1,'1',0)]
-}
liftDelta::(Ord a) => [(a,Symbol,a)] -> Delta a
liftDelta ds = liftL $ fmap tupleAUX ds

{-|
Transition function that for every pair, a State and a Symbol by domain, decide
next list of states in machine
-}
type NDelta a = (:*>:) a Symbol (SetLabel a)

{-|
Lift a list of 3-tuples to a non deterministic delta

>>>let deltaN = liftNDelta [(0,'0',[0]),(0,'1',[1]),(1,'0',[1]),(1,'1',[0])]
-}
liftNDelta::(Ord a) => [(a,Symbol,[a])] -> NDelta a
liftNDelta ds = let
		(xs, ys, wss) = unzip3 ds
		f ws = Set.fromList (map return ws)
	in liftL $ zip3 xs ys $ fmap f wss

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
liftL1 = liftL . fmap tupleMidVoid

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
	F (Delta a) (Final a) (Label a)
	-- |>>>let autFinN = FN deltaN (Set.fromList [Q 0]) (Q 0)
	| FN (NDelta a) (Final a) (Label a) deriving(Show,Eq)

{-|
Gets alphabet for some finite automaton
-}
getAlphabet:: FiniteA a -> Alphabet
getAlphabet (F d _ _)   = getParamDomainSet d
getAlphabet (FN dn _ _) = getParamDomainSet dn

getAlphabetList::FiniteA a -> [Symbol]
getAlphabetList (F d _ _)   = getParamDomain d
getAlphabetList (FN dn _ _) = getParamDomain dn

{-|
For some delta, an initial state anf a word returns final state for that word
-}
nextState:: (Ord a) => Delta a -> Wd -> State (Label a) (Label a)
nextState _ [] = get
nextState d (x:xs) = do
	q <- get
	if q==QE
		then return q
		else do
			put (nextOutput d QE (q,x))
			nextState d xs

nextStates' dt = nextOutput dt (Set.singleton QE)

mapNextStates dt x = Set.map (\q -> nextStates' dt (q,x))

{-|
Same as nextState but work with no deterministic delta
-}
nextStates::(Ord a) => NDelta a -> Wd -> State (SetLabel a) (SetLabel a)
nextStates _ [] = get
nextStates dn (x:xs) = do
	sq <- get
	if Set.null sq
		then return (Set.singleton QE)
		else do
			put ((Set.delete QE . unionsFold) (mapNextStates dn x sq))
			nextStates dn xs

{-|
Executes a automaton over a word

>>>checkString autFin "1010010101101010"
True
>>>checkString autFin "1010010101101010001010101010"
False
-}
checkString::(Ord a) => FiniteA a -> Wd -> Bool
checkString (F d qF s) ws = let
		q = evalState (nextState d ws) s
		f y = (not.isError) y && terminal qF y
	in f q
checkString (FN dn qF s) ws = let
		sq = evalState (nextStates dn ws) (Set.fromList [s])
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
	Moore (Delta a) (Lambda1 a) (Label a)
	|Mealy (Delta a) (Lambda2 a) (Label a) deriving(Show, Eq)

{-|
Extends nextState function
-}
transMoore:: (Ord a) => Delta a -> Lambda1 a -> Wd -> State (Wd, Label a) Wd
transMoore _ l [] = do
	(ws, q) <- get
	if q==QE
		then return ws
		else return $ ws++[nextSymbol l (q, ())]
transMoore d l (x:xs) = do
	(ws,q) <- get
	if q==QE
		then return ws
		else do
			put (ws++[nextSymbol l (q, ())], nextOutput d QE (q,x))
			transMoore d l xs

{-|
Extends nextState function
-}
transMealy:: (Ord a) => Delta a -> Lambda2 a -> Wd -> State (Wd, Label a) Wd
transMealy _ _ [] = do
	(ws, q) <- get
	return ws
transMealy d l (x:xs) = do
	(ws, q) <- get
	if q==QE
		then return ws
		else do
			put (ws++[nextSymbol l (q, x)], nextOutput d QE (q,x))
			transMealy d l xs

{-|
For every transducer, given a word the automaton change all symbols in lambda
-}
translate::(Ord a) => Transductor a -> Wd -> Wd
translate (Moore d l s) ws = let
		(nws, _) = runState (transMoore d l ws) ([], s)
	in nws
translate (Mealy d l s) ws = let
		(nws, _) = runState (transMealy d l ws) ([], s)
	in nws

{-|
Gets alphabet for some finite automaton
-}
getOutputAlphabet:: Transductor a -> Alphabet
getOutputAlphabet (Moore _ l _) = getRangeSet l
getOutputAlphabet (Mealy _ l _) = getRangeSet l

{-|
Transforms a Transducer to a transducer Autmaton
-}
transducerToFinite:: Transductor a -> Final a -> FiniteA a
transducerToFinite (Moore d _ s) qf = F d qf s
transducerToFinite (Mealy d _ s) qf = F d qf s

{-|
Transforms a Finite Autmaton with some lambda to a Moore Transducer
-}
finiteToMoore:: (Enum a, Ord a) => FiniteA a -> Lambda1 a -> Transductor a
finiteToMoore (F d _ s) l = Moore d l s
finiteToMoore fn l = finiteToMoore (convertFA fn) l

{-|
Transforms a Finite Autmaton with some lambda to a Mealy Transducer
-}
finiteToMealy:: (Enum a, Ord a) => FiniteA a -> Lambda2 a -> Transductor a
finiteToMealy (F d _ s) l = Mealy d l s
finiteToMealy fn l = finiteToMealy (convertFA fn) l

reachableStates1 alp d xs = let
    qs = xs ++ [nextOutput d QE (y,x) | x<-alp, y<-xs]
    nqs = (\\) (nub qs) [QE]
  in
    if nqs==xs then nqs else reachableStates1 alp d nqs

reachableStates' alp d sq = let
		f a q = Set.singleton (nextOutput d QE (q,a))
		g a = Fold.foldMap (f a) sq
		sq' = Set.union sq $ Fold.foldMap g alp
		nsq = Set.delete QE sq'
	in
		if nsq==sq then nsq else reachableStates' alp d nsq

reachableStates2 alp d xs = let
    qs = (xs ++ concat [Set.toList (nextStates' d (y,x)) | x<-alp, y<-xs])\\[QE]
    nqs = nub qs
  in
    if nqs==xs then nqs else reachableStates2 alp d nqs

{-|
Minimaize a delta for some finite automaton.
Gets a delta with all reachable states from initial state.
-}
reachableFinite::(Ord a) => FiniteA a -> FiniteA a
reachableFinite af@(F d sqf qi) = let
		alp = getAlphabet af
		sq = reachableStates' alp d (Set.singleton qi)
		allUnused = (Set.\\) (getStateDomainSet d) sq
		sk = Fold.foldMap (\a -> Fold.foldMap (\q -> Set.singleton (q,a)) allUnused) alp
    --ks = [(x,y) | x<-allUnused, y<-alp]
		nDelta = Fold.foldl (flip Map.delete) d sk
	in
		F nDelta (Set.intersection sqf sq) qi
reachableFinite afn@(FN dn sqf qi) = let
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
    g p q a = eqD (nextOutput d QE (p, a)) (nextOutput d QE (q, a))
    f p q = Fold.all (g p q) alp
    (sx, sy) = Set.partition (f qM) pi
  in Set.delete Set.empty $ Set.fromList [sx, sy]

distinguishableSet2 alp nd partSet pi = let
    qM = Set.findMin pi
    eqD p q = (==) (partitionSet2 p partSet) (partitionSet2 q partSet)
    g p q a = eqD (nextStates' nd (p, a)) (nextStates' nd (q, a))
    f p q = Fold.all (g p q) alp
    (sx, sy) = Set.partition (f qM) pi
  in Set.delete Set.empty $ Set.fromList [sx, sy]

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

allStateSet (F d sqf q0) = Set.unions [getRangeSet d, getStateDomainSet d, sqf, Set.singleton q0]
allStateSet (FN nd sqf q0) = Set.unions [(Set.unions . Map.elems) nd, getStateDomainSet nd, sqf, Set.singleton q0]

{-|
Delete redundant states and their transitions, if a state is equivalent to
another then is redundant, two state are equivalent if they are
undistinguisahbles.
-}
distinguishableFinite::(Ord a) => FiniteA a -> FiniteA a
distinguishableFinite af@(F d sf si) = let
    allState = allStateSet af
    pInitSet = fstPartitionSet sf allState
    alp = getAlphabet af
    partSet = lDistinguishableSet alp d pInitSet
    f q = (Set.findMin . Set.findMin) $ partitionSet q partSet
    allNewStateSet = Set.map f allState
    g q delta a = let
        k = (q, a)
        nQ = nextOutput d QE k
      in if nQ==QE
        then delta
        else Map.insert k (f nQ) delta
    h delta q = Fold.foldl (g q) delta alp
    newDelta = Fold.foldl h Map.empty allNewStateSet
  in
    F newDelta (Set.map f sf) (f si)
distinguishableFinite afn@(FN nd sf si) = let
    allState = allStateSet afn
    pInitSet = fstPartitionSet sf allState
    alp = getAlphabet afn
    partSet = lDistinguishableSet2 alp nd pInitSet
    f q = (Set.findMin . Set.findMin) $ partitionSet q partSet
    allNewStateSet = Set.map f allState
    g q ndelta a = let
        k = (q, a)
        nQ = nextStates' nd k
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
minimizeFinite = reachableFinite . distinguishableFinite

state2Set::(Ord a) => Label a -> Set.Set a
state2Set QE    = Set.empty
state2Set (Q x) = Set.fromList [x]

setState2Set'::(Ord a) => Set.Set a -> SetLabel a -> Set.Set a
setState2Set' sa sP = if sP==Set.empty
  then sa
  else let
      p = Set.elemAt 0 sP
    in setState2Set' (Set.union (state2Set p) sa) (Set.delete p sP)

setState2Set::(Ord a) => SetLabel a -> Set.Set a
setState2Set = setState2Set' Set.empty

nextStateSet::(Ord a) => NDelta a -> LabelSS a -> Symbol -> SetLabel a
nextStateSet nd qsq a = let
    f q = nextStates' nd (q, a)
    g = Set.map f
    Q sq = fmap g qsq
  in unionsFold sq

updateDeltaBySym::(Ord a) => NDelta a -> LabelSS a -> Symbol -> Delta (SetLabel a) -> Delta (SetLabel a)
updateDeltaBySym nd qsq a d = let
    k = (qsq, a)
    psp = Q $ nextStateSet nd qsq a
  in Map.insert k psp d

updateDeltaByState::(Ord a) => NDelta a -> LabelSS a -> Delta (SetLabel a) -> Delta (SetLabel a)
updateDeltaByState nd qsq delta = let
    f d a = updateDeltaBySym nd qsq a d
  in Fold.foldl f delta (getParamDomainSet nd)

updateDelta::(Ord a) => NDelta a -> LabelSS a -> Delta (SetLabel a) -> Delta (SetLabel a)
updateDelta nd qsq d = let
    dDom = getStateDomainSet d
    newD = updateDeltaByState nd qsq d
    newDDom = getRangeSet newD
    difS = Set.difference (Set.difference newDDom dDom) (Set.fromList [qsq])
    f delta psp = updateDelta nd psp delta
  in if Set.null difS
    then newD
    else Fold.foldl f newD difS

isNewFinal::(Ord a) => Set.Set a -> LabelSS a -> Bool
isNewFinal _ QE = False
isNewFinal sa (Q sq) = let
    sInter = Set.intersection sa (setState2Set sq)
  in not $ Set.null sInter

convertFA'::(Ord a) => FiniteA a -> FiniteA (SetLabel a)
convertFA' (FN nd sqf q0) = let
    alp = getParamDomainSet nd
    newQ0 = Q $ Set.singleton q0
    newD = updateDelta nd newQ0 Map.empty
    sf = setState2Set sqf
    dDom = Set.unions [getStateDomainSet newD, getRangeSet newD, Set.singleton newQ0]
    newSQF = Set.filter (isNewFinal sf) dDom
  in minimizeFinite $ F newD newSQF newQ0

enumDom::(Ord a) => Set.Set (LabelSS a) -> LabelSS a -> Int
enumDom sqsq qsq = Set.findIndex qsq sqsq

succN:: (Enum a) => a -> Int -> a
succN a 0 = a
succN a n = succN (succ a) (n-1)

newLabel::(Enum a, Ord a) => a -> Set.Set (LabelSS a) -> LabelSS a -> Label a
newLabel o sqsq qsq = Q . succN o $ enumDom sqsq qsq

mapSetLabel::(Enum a, Ord a) => a -> Set.Set (LabelSS a) -> Set.Set (LabelSS a) -> Set.Set (Label a)
mapSetLabel o sqsq = Set.map $ newLabel o sqsq

mapDeltaLabel::(Enum a, Ord a) => a -> Set.Set (LabelSS a) -> Delta (SetLabel a) -> Delta a
mapDeltaLabel o sqsq rareD = let
		f (qsq, x) = (newLabel o sqsq qsq, x)
		g = newLabel o sqsq
  in Map.mapKeys f (Map.map g rareD)

state2Enum::(Enum a) => Label a -> a
state2Enum QE    = toEnum 0
state2Enum (Q a) = a

mapAFLabel::(Enum a, Ord a) => Label a -> FiniteA (SetLabel a) -> FiniteA a
mapAFLabel q af@(F d sqf q0) = let
    o = state2Enum q
    sqsq = allStateSet af
  in F (mapDeltaLabel o sqsq d) (mapSetLabel o sqsq sqf) (newLabel o sqsq q0)

{-|
Finite Autmaton Equivalence
-}
convertFA::(Enum a, Ord a) => FiniteA a -> FiniteA a
convertFA (F d sqf q0) = let
    f x = Set.singleton x
  in
    FN (fmap f d) sqf q0
convertFA afn@(FN nd sqf q0) = let
    afRare = convertFA' afn
  in
    mapAFLabel q0 afRare

{-|
Finite Transducer Autmaton Similarity
-}
convertTA::(Ord a) => Transductor a -> Transductor a
convertTA (Moore d l s) = let
		f _ QE nl = nl
		f (p,a) q nl = Map.insert (p,a) (nextSymbol l (q, ())) nl
	in Mealy d (Map.foldrWithKey f Map.empty d) s
convertTA (Mealy d l s) = let
		f _ QE nl = nl
		f (p,a) q nl = Map.insert (q,()) (nextSymbol l (p,a)) nl
	in Moore d (Map.foldrWithKey f Map.empty d) s
	--bad definition


{-|
Tells if a finite automaton had empty language or not.
-}
automatonEssence:: (Ord a) => FiniteA a -> Essence
automatonEssence af@F{} = let
    (F d sqf q0) = reachableFinite af
    rangeD = getRangeSet d
  in if Set.null (Set.intersection rangeD sqf) && Set.notMember q0 sqf
    then Empty
    else Occupied
automatonEssence af@FN{} = let
    (FN nd sqf q0) = reachableFinite af
    rangeD = (Set.unions . Map.elems) nd
  in if Set.null (Set.intersection rangeD sqf) && Set.notMember q0 sqf
    then Empty
    else Occupied

acceptWord _ []      = False
acceptWord af (w:ws) = checkString af w || acceptWord af ws

allStateSize s = setGenericSize $ allStateSet s

filterWords af = filter (checkString af)

{-|
Tells if a finite automaton had infinite language or the number or words in his
language
-}
automatonCardinality::(Ord a) => FiniteA a -> Discrete
automatonCardinality af = let
    afm = minimizeFinite af
    alp = getAlphabet afm
    n = allStateSize afm
    g = kWords alp
    acceptedWord = acceptWord afm $ g =<< [n..(2*(n-1))]
  in if acceptedWord
    then Numerable
    else Fin . genericLength . filterWords afm $ lessKWords alp (n-1)
