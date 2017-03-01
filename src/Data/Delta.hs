{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE TypeOperators #-}
{-|
Module      : Delta
Description : Partial functions
Copyright   : (c) Jorge Santiago Alvarez Cuadros, 2016
License     : GPL-3
Maintainer  : sanjorgek@ciencias.unam.mx
Stability   : experimental
Portability : portable

Map implementation, represent a partial function
-}
module Data.Delta
(
	-- * Delta
	-- ** Deterministic
	-- *** Constructor
	(:->:)(..)
	-- *** Functions
  ,liftD
	,nextD
	-- ** Not deterministic
	-- *** Constructor
	,(:-<:)(..)
  -- *** Functions
  ,liftND
  ,nextND
	-- * Transductor
	-- ** Constructor
	,(:*>:)(..)
  -- ** Functions
  ,liftL
  ,nextTMaybe
  ,nextT
  -- * Auxiliar functions
  ,getFirstParam
  ,getSecondParam
  ,getStateDomain
  ,getStateRange
  ,getStateRangeD
  ,getStateRangeND
  ,getFirstParamSet
  ,getSecondParamSet
  ,getStateDomainSet
  ,getStateRangeSet
  ,getStateRangeSetD
  ,getStateRangeSetND
) where
import qualified Data.Foldable   as Fold
import           Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import           Data.Sigma
import           Data.State
import           Data.Maybe

{-|
Map a tuple, a state and a param, to some output
-}
type (:*>:) a p o = Map.Map (State a, p) o

liftL :: (Ord a, Ord p) => [(a, p, o)] -> (:*>:) a p o
liftL ds = let
    (xs, ys, zs) = unzip3 ds
    f = map return
    nds = zip (zip (f xs) ys) zs
  in Map.fromList nds

nextTMaybe :: (Ord p1, Ord a) => (:*>:) a p1 o -> (State a, p1) -> Maybe o
nextTMaybe dt k = if Map.member k dt 
  then Just $ dt Map.! k
  else Nothing

{-|
For simple map with Chars range
-}
nextT::(Ord p1, Ord a) => (:*>:) a p1 Symbol -> (State a, p1) -> Symbol
nextT dt k = let
    mO = nextTMaybe dt k
  in if isJust mO
    then fromJust mO
    else '\0'

{-|
Deterministic Delta

Maps a tuple, a state and a param, to a tuple, a state and a param.
-}
type (:->:) a p1 p2 = (:*>:) a p1 (State a, p2)

liftD::(Ord a, Ord p1) => [(a, p1, a, p2)] -> (:->:) a p1 p2
liftD ds = let
    (xs, ys, ws, zs) = unzip4 ds
    as = zip (map return ws) zs
  in liftL $ zip3 xs ys as

{-|
Next state function for deterministic delta
-}
nextD :: (Ord p1, Ord a) => (:->:) a p1 p2 -> (State a, p1) -> State a
nextD dt k = let
    mQ = nextTMaybe dt k
  in if isJust mQ
    then fst $ fromJust mQ
    else QE

{-|
Non-Deterministic Delta

Maps a tuple, a state and a param, to a tuple, a state list and a param.
-}
type (:-<:) a p1 p2 = (:*>:) a p1 (Set.Set (State a), p2)

liftND::(Ord a, Ord p1) => [(a, p1, [a], p2)] -> (:-<:) a p1 p2
liftND ds = let
    (xs, ys, wss, zs) = unzip4 ds
    f = Set.fromList . map return
    as = zip (map f wss) zs
  in liftL $ zip3 xs ys as

{-|
Next state function for non-deterministic delta
-}
nextND :: (Ord p1, Ord a) => (:-<:) a p1 p2 -> (State a, p1) -> Set.Set (State a)
nextND dt k = let
    mQ = nextTMaybe dt k
  in if isJust mQ 
    then fst $ fromJust mQ
    else Set.singleton QE

{-|
Gets all params at domain, for (:->:) and (:-<:)
-}
getFirstParam::(Eq b) => Map.Map (a, b) a1 -> [b]
getFirstParam = nub . map snd . Map.keys

getFirstParamSet::(Ord b) => Map.Map (a, b) a1 -> Set.Set b
getFirstParamSet = Set.fromList . map snd . Map.keys

{-|
Gets all params at range, for (:->:) and (:-<:)
-}
getSecondParam::(Eq b) => Map.Map k (a, b) -> [b]
getSecondParam = nub . map snd . Map.elems

getSecondParamSet::(Ord b) => Map.Map k (a, b) -> Set.Set b
getSecondParamSet = Set.fromList . map snd . Map.elems

{-|
Gets all states at domain, for (:->:) and (:-<:)
-}
getStateDomain::(Eq a) => Map.Map (a, b) a1 -> [a]
getStateDomain = nub . map fst . Map.keys

getStateDomainSet::(Ord a) => Map.Map (a, b) a1 -> Set.Set a
getStateDomainSet = Set.fromList . map fst . Map.keys

{-|
Gets all params at range, for (:->:) and (:-<:)
-}
getStateRange::(Eq a) => Map.Map k (a, b) -> [a]
getStateRange = nub . map fst . Map.elems

getStateRangeSet::(Ord a) => Map.Map k (a, b) -> Set.Set a
getStateRangeSet = Set.fromList . map fst . Map.elems

getStateRangeD::(Eq a) => (:->:) a p1 p2 -> [State a]
getStateRangeD = getStateRange

getStateRangeSetD::(Ord a) => (:->:) a p1 p2 -> Set.Set (State a)
getStateRangeSetD = getStateRangeSet

getStateRangeND::(Ord a) => (:-<:) a p1 p2 -> [State a]
getStateRangeND = Set.toList . Set.unions . map fst . Map.elems

getStateRangeSetND::(Ord a) => (:-<:) a p1 p2 -> Set.Set (State a)
getStateRangeSetND = Set.unions . map fst . Map.elems
