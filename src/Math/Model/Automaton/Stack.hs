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
  ,Key(..)
  ,liftD
	,StackA(..)
  ,getInputAlphabet
  ,getSigma
  ,getStackAlphabet
  ,checkWordByStack
) where
import           Data.Delta
import qualified Data.Foldable   as Fold
import           Data.List
import qualified Data.Map.Strict as Map
import           Data.Monoid
import           Data.Sigma
import           Data.State

{-|
Delta for stack machine, takes a state, a symbol in string input or not and a
symbol in stack head and returns next state and update stack
-}
type Delta a = (:->:) a (Either Symbol Epsilon, Symbol) Wd

{-|
A key for a delta.
-}
type Key a = (State a, (Either Symbol Epsilon, Symbol))

{-|
Takes a list of tuples and lift a Delta

>>>let delta = liftD [(0,"(",'Z',0,"IZ"),(0,"",'Z',0,""),(0,"(",'I',0,"II"),(0,")",'I',0,"")]
-}
liftD:: Ord a => [(a, Wd, Symbol, a, Wd)]-> Delta a
liftD xs = let
    (as,bs,cs,ds,es) = unzip5 xs
    f = map Q
    g [] = Right Epsilon
    g (x:_) = Left x
    ps = zip (map g bs) cs
    ks = zip (f as) ps
    rs = zip (f ds) es
  in Map.fromList (zip ks rs)

nextDTuple :: Ord a => Delta a -> Key a -> (State a, Wd)
nextDTuple dt k = if Map.member k dt then dt Map.! k else (QE,[])

-- |Stack machine only needs a delta, an init state and an initial symbol.
--
-- This works for empty stack and final state acceptor
data StackA a = Stack (Delta a) (State a) (Final a) Symbol deriving(Show, Eq)

getSigma :: [Either a b] -> [a]
getSigma [] = []
getSigma (Left x : xs) = x : getSigma xs
getSigma (_:xs) = getSigma xs

getInputAlphabet :: StackA a -> [Either Symbol Epsilon]
getInputAlphabet (Stack dn _ _ _) = (fst . unzip . getFirstParam) dn

getStackAlphabet :: StackA a -> [Symbol]
getStackAlphabet (Stack dn _ _ _) = (snd . unzip . getFirstParam) dn

cleanStacks::[(State a, Wd)]->[(State a, Wd)]
cleanStacks [] = []
cleanStacks ((QE, _):xs) = cleanStacks xs
cleanStacks (x:xs) = x : cleanStacks xs

cleanStacks2::[(State a, Wd)]->[(State a, Wd)]
cleanStacks2 [] = []
cleanStacks2 ((QE, _):xs) = cleanStacks2 xs
cleanStacks2 ((_, []):xs) = cleanStacks xs
cleanStacks2 (x:xs) = x : cleanStacks2 xs

aceptEmptyStack::[(State a,Wd)]->Bool
aceptEmptyStack = any aceptF1
  where
    aceptF1 (QE,_) = False
    aceptF1 (_,[]) = True
    aceptF1 (_,_) = False

epsilonTrans:: Ord a => Delta a -> State a -> Symbol -> (State a, Wd)
epsilonTrans dn q b = nextDTuple dn (q, (Right Epsilon, b))

symbolTrans:: Ord a => Delta a -> State a -> Symbol -> Symbol -> (State a, Wd)
symbolTrans dn q a b = nextDTuple dn (q, (Left a, b))

mapEpsilon:: Ord a => Delta a -> [(State a, Wd)] -> [(State a, Wd)]
mapEpsilon dn [] = []
mapEpsilon dn ((q,b:bs):xs) = let
    (p,ys) = epsilonTrans dn q b
  in (p, ys++bs) : mapEpsilon dn xs

mapSymbol:: Ord a => Delta a -> Symbol -> [(State a, Wd)] -> [(State a, Wd)]
mapSymbol dn _ [] = []
mapSymbol dn a ((q,b:bs):xs) = let
    (p, ys) = symbolTrans dn q a b
  in (p, ys++bs) : mapSymbol dn a xs

checkWordByStack:: Ord a => StackA a -> Wd -> Bool
checkWordByStack (Stack dn qi _ z0) ws = let
    checkString [] _ = False
    checkString stks [] = aceptEmptyStack stks || checkString ((cleanStacks . mapEpsilon dn) stks) []
    checkString stks (a:as) = let
        stks2 = (cleanStacks2 . mapEpsilon dn) stks
      in checkString ((cleanStacks . mapSymbol dn a) (stks++stks2)) as
  in checkString [(qi,[z0])] ws

aceptState::(Ord a) => Final a -> [(State a,Wd)] -> Bool
aceptState qfs = any aceptF1
  where
    aceptF1 (QE,_) = False
    aceptF1 (q,_) = terminal qfs q

checkWordByFinal (Stack dn qi qfs z0) ws = let
    checkString [] _ = False
    checkString stks [] = aceptState qfs stks || checkString ((cleanStacks . mapEpsilon dn) stks) []
    checkString stks (a:as) = let
        stks2 = (cleanStacks2 . mapEpsilon dn) stks
      in checkString ((cleanStacks . mapSymbol dn a) (stks++stks2)) as
  in checkString [(qi, [z0])] ws
