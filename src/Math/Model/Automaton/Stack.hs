{-# OPTIONS_GHC -fno-warn-tabs      #-}
{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE TypeOperators          #-}
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
module Math.Model.Automaton.Stack where
import           Data.Delta
import qualified Data.Foldable   as Fold
import           Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Sigma
import           Data.Label
import           Control.Monad.State.Lazy

{-|
Delta for stack machine, takes a state, a symbol in string input or not and a
symbol in stack head and returns next state and update stack
-}
type DeltaS a = (:->:) a (Symbol, Symbol) Wd
type DeltaE a = (:->:) a (Maybe Symbol, Symbol) Wd
type DeltaN a = (:->:) a (Maybe Symbol, Symbol) (Set.Set Wd)

{-|
A key for a delta.
-}
type KeyS a = (Label a, (Symbol, Symbol))
type KeyE a = (Label a, (Maybe Symbol, Symbol))

{-|
Takes a list of tuples and lift a Delta

>>>let delta = liftDeltaSimple [(0,'(','Z',0,"A"),(0,'a','0',0,"AA"),(0,'b','A',0,""),(1,'b','A',1,"")]
-}
liftDeltaSimple:: Ord a => [(a, Symbol, Symbol, a, Wd)]-> DeltaS a
liftDeltaSimple xs = let
    (as,bs,cs,ds,es) = unzip5 xs
    f = fmap Q
    ps = zip bs cs
    ks = zip (f as) ps
    rs = zip (f ds) es
  in Map.fromList (zip ks rs)

{-|
Takes a list of tuples and lift a Delta

>>>let delta = liftDeltaEpsilon [(0,"(",'Z',0,"IZ"),(0,"",'Z',0,""),(0,"(",'I',0,"II"),(0,")",'I',0,"")]
-}
liftDeltaEpsilon:: Ord a => [(a, Wd, Symbol, a, Wd)]-> DeltaE a
liftDeltaEpsilon xs = let
    (as,bs,cs,ds,es) = unzip5 xs
    f = fmap Q
    g [] = Nothing
    g (x:_) = Just x
    ps = zip (fmap g bs) cs
    ks = zip (f as) ps
    rs = zip (f ds) es
  in Map.fromList (zip ks rs)

nextDTuple :: Ord a => DeltaE a -> KeyE a -> (Label a, Wd)
nextDTuple dt k = if Map.member k dt then dt Map.! k else (QE,[])

-- |Stack machine only needs a delta, an init state and an initial symbol.
--
-- This works for empty stack and final state acceptor
data StackA a =
  StackS {
    getDeltaSimple::DeltaS a
    ,getInitState::Label a
    ,getFinal::Final a
    ,getInitSymbol::Symbol}
  | StackE {
    getDeltaEpsilon::DeltaE a
    ,getInitState::Label a
    ,getFinal::Final a
    ,getInitSymbol::Symbol} deriving(Show, Eq)

nextStateSimple::(Ord a) => DeltaS a -> Wd -> State (Wd, Label a) (Label a)
nextStateSimple _ [] = do
	(_, q) <- get
	return q
