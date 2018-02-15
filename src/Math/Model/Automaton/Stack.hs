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
import           Data.Sigma
import           Data.Label
import           Control.Monad.State.Lazy

{-|
Delta for stack machine, takes a state, a symbol in string input or not and a
symbol in stack head and returns next state and update stack
-}
type Delta a = (:->:) a (Maybe Symbol, Symbol) Wd

{-|
A key for a delta.
-}
type Key a = (Label a, (Maybe Symbol, Symbol))

{-|
Takes a list of tuples and lift a Delta

>>>let delta = liftD [(0,"(",'Z',0,"IZ"),(0,"",'Z',0,""),(0,"(",'I',0,"II"),(0,")",'I',0,"")]
-}
liftDelta:: Ord a => [(a, Wd, Symbol, a, Wd)]-> Delta a
liftDelta xs = let
    (as,bs,cs,ds,es) = unzip5 xs
    f = fmap Q
    g [] = Nothing
    g (x:_) = Just x
    ps = zip (fmap g bs) cs
    ks = zip (f as) ps
    rs = zip (f ds) es
  in Map.fromList (zip ks rs)

nextDTuple :: Ord a => Delta a -> Key a -> (Label a, Wd)
nextDTuple dt k = if Map.member k dt then dt Map.! k else (QE,[])

-- |Stack machine only needs a delta, an init state and an initial symbol.
--
-- This works for empty stack and final state acceptor
data StackA a = Stack {
  getDelta::Delta a
  ,getInitState::Label a
  ,getFinal::Final a
  ,getInitSymbol::Symbol} deriving(Show, Eq)

nextState::(Ord a) => Delta a -> Wd -> State (Wd, Label a) (Label a)
nextState _ [] = do
	(_, q) <- get
	return q
