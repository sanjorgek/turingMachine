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
  -- * Function
  Delta(..)
  ,liftDelta
  ,Key(..)
  -- * Constructor
	,StackA(..)
  ,checkWordByStack
  ,checkWordByFinal
  -- * Auxiliar functions
  ,getInputAlphabet
  ,getSigma
  ,getStackAlphabet
) where
import           Data.Delta
import qualified Data.Foldable   as Fold
import           Data.List
import qualified Data.Map.Strict as Map
import           Data.Sigma
import           Data.Label

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
    f = map Q
    g [] = Nothing
    g (x:_) = Just x
    ps = zip (map g bs) cs
    ks = zip (f as) ps
    rs = zip (f ds) es
  in Map.fromList (zip ks rs)

nextDTuple :: Ord a => Delta a -> Key a -> (Label a, Wd)
nextDTuple dt k = if Map.member k dt then dt Map.! k else (QE,[])

-- |Stack machine only needs a delta, an init state and an initial symbol.
--
-- This works for empty stack and final state acceptor
data StackA a = Stack (Delta a) (Label a) (Final a) Symbol deriving(Show, Eq)

{-|
Return only real input character at delta definition
-}
getSigma :: [Maybe a] -> [a]
getSigma [] = []
getSigma (Just x : xs) = x : getSigma xs
getSigma (_:xs) = getSigma xs

-- |Gives a input character, included epsilon
getInputAlphabet :: StackA a -> [Maybe Symbol]
getInputAlphabet (Stack dn _ _ _) = (fst . unzip . getFirstParam) dn

-- |Gives a stack alphabet
getStackAlphabet :: StackA a -> [Symbol]
getStackAlphabet (Stack dn _ _ _) = (snd . unzip . getFirstParam) dn

cleanStacks::[(Label a, Wd)]->[(Label a, Wd)]
cleanStacks [] = []
cleanStacks ((QE, _):xs) = cleanStacks xs
cleanStacks (x:xs) = x : cleanStacks xs

cleanStacks2::[(Label a, Wd)]->[(Label a, Wd)]
cleanStacks2 [] = []
cleanStacks2 ((QE, _):xs) = cleanStacks2 xs
cleanStacks2 ((_, []):xs) = cleanStacks xs
cleanStacks2 (x:xs) = x : cleanStacks2 xs

aceptEmptyStack::[(Label a,Wd)]->Bool
aceptEmptyStack = any aceptF1
  where
    aceptF1 (QE,_) = False
    aceptF1 (_,[]) = True
    aceptF1 (_,_) = False

epsilonTrans:: Ord a => Delta a -> Label a -> Symbol -> (Label a, Wd)
epsilonTrans dn q b = nextDTuple dn (q, (Nothing, b))

symbolTrans:: Ord a => Delta a -> Label a -> Symbol -> Symbol -> (Label a, Wd)
symbolTrans dn q a b = nextDTuple dn (q, (Just a, b))

mapEpsilon:: Ord a => Delta a -> [(Label a, Wd)] -> [(Label a, Wd)]
mapEpsilon dn [] = []
mapEpsilon dn ((q,b:bs):xs) = let
    (p,ys) = epsilonTrans dn q b
  in (p, ys++bs) : mapEpsilon dn xs

mapSymbol:: Ord a => Delta a -> Symbol -> [(Label a, Wd)] -> [(Label a, Wd)]
mapSymbol dn _ [] = []
mapSymbol dn a ((q,b:bs):xs) = let
    (p, ys) = symbolTrans dn q a b
  in (p, ys++bs) : mapSymbol dn a xs

{-|
Given a stack automaton check if some word is acepted by empty stack.
-}
checkWordByStack:: Ord a => StackA a -> Wd -> Bool
checkWordByStack (Stack dn qi _ z0) ws = let
    checkString [] _ = False
    checkString stks [] = aceptEmptyStack stks || checkString ((cleanStacks . mapEpsilon dn) stks) []
    checkString stks (a:as) = let
        stks2 = (cleanStacks2 . mapEpsilon dn) stks
      in checkString ((cleanStacks . mapSymbol dn a) (stks++stks2)) as
  in checkString [(qi,[z0])] ws

aceptState::(Ord a) => Final a -> [(Label a,Wd)] -> Bool
aceptState qfs = any aceptF1
  where
    aceptF1 (QE,_) = False
    aceptF1 (q,_) = terminal qfs q

{-|
Given a stack automaton check if some word is acepted by final state.
-}
checkWordByFinal (Stack dn qi qfs z0) ws = let
    checkString [] _ = False
    checkString stks [] = aceptState qfs stks || checkString ((cleanStacks . mapEpsilon dn) stks) []
    checkString stks (a:as) = let
        stks2 = (cleanStacks2 . mapEpsilon dn) stks
      in checkString ((cleanStacks . mapSymbol dn a) (stks++stks2)) as
  in checkString [(qi, [z0])] ws
