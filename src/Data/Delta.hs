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
  -- ** Generic
	(:*>:)(..)
	-- ** Deterministic
	-- *** Constructor
	,(:->:)(..)
	-- *** Functions
  ,liftD
	,nextD
	-- ** Not deterministic
	-- *** Constructor
	,(:-<:)(..)
  -- *** Functions
  ,liftND
  ,nextND
  -- ** Functions
  ,liftL
  ,nextTMaybe
  ,nextSymbol
  -- * Auxiliar functions
  ,getFirstParam
  ,getFirstParamSet
  ,getSecondParam
  ,getSecondParamSet
  ,getStateDomain
  ,getStateDomainSet
  ,getStateRangeD
  ,getStateRangeND
  ,getStateRangeSetD
  ,getStateRangeSetND
) where
import qualified Data.Foldable   as Fold
import           Data.Label
import           Data.List
import qualified Data.Map.Strict as Map
import           Data.Maybe
import qualified Data.Set        as Set
import           Data.Sigma

{-|
Map a tuple, a state and a param, to some output
-}
type (:*>:) a p o = Map.Map (Label a, p) o

{-|
Lift a generic delta/map from a 3-tuple list
-}
liftL :: (Ord a, Ord p) => [(a, p, o)] -> (:*>:) a p o
liftL ds = let
    (xs, ys, zs) = unzip3 ds
    f = map return
    nds = zip (zip (f xs) ys) zs
  in Map.fromList nds

{-|
Take a state and a param and maybe resolve some output
-}
nextTMaybe :: (Ord p1, Ord a) => (:*>:) a p1 o -> (Label a, p1) -> Maybe o
nextTMaybe dt k = if Map.member k dt
  then Just $ dt Map.! k
  else Nothing

{-|
For simple map with Chars range
-}
nextSymbol::(Ord p1, Ord a) => (:*>:) a p1 Symbol -> (Label a, p1) -> Symbol
nextSymbol dt k = let
    mO = nextTMaybe dt k
  in fromMaybe '\NUL' mO

{-|
Deterministic Delta

Maps a tuple, a state and a param, to another tuple, a state and a param.
-}
type (:->:) a p1 p2 = (:*>:) a p1 (Label a, p2)

{-|
Lifts a deterministic delta from a 4-tuple list
-}
liftD::(Ord a, Ord p1) => [(a, p1, a, p2)] -> (:->:) a p1 p2
liftD ds = let
    (xs, ys, ws, zs) = unzip4 ds
    as = zip (map return ws) zs
  in liftL $ zip3 xs ys as

{-|
Next state function for deterministic delta
-}
nextD :: (Ord p1, Ord a) => (:->:) a p1 p2 -> (Label a, p1) -> Label a
nextD dt k = let
    mQ = nextTMaybe dt k
  in maybe QE fst mQ

{-|
Non-Deterministic Delta

Maps a tuple, a state and a param, to a tuple, a state list and a param.
-}
type (:-<:) a p1 p2 = (:*>:) a p1 (Set.Set (Label a), p2)

{-|
Lifts a non-deterministic delta from a 4-tuple list
-}
liftND::(Ord a, Ord p1) => [(a, p1, [a], p2)] -> (:-<:) a p1 p2
liftND ds = let
    (xs, ys, wss, zs) = unzip4 ds
    f = Set.fromList . map return
    as = zip (map f wss) zs
  in liftL $ zip3 xs ys as

{-|
Next state function for non-deterministic delta
-}
nextND :: (Ord p1, Ord a) => (:-<:) a p1 p2 -> (Label a, p1) -> Set.Set (Label a)
nextND dt k = let
    mQ = nextTMaybe dt k
  in maybe (Set.singleton QE) fst mQ

{-|
Gets all params at domain, for (:->:)
-}
getFirstParam::(Eq b) => Map.Map (a, b) a1 -> [b]
getFirstParam = nub . map snd . Map.keys

{-|
Gets all params at domain, for (:-<:)
-}
getFirstParamSet::(Ord b) => Map.Map (a, b) a1 -> Set.Set b
getFirstParamSet = Set.fromList . map snd . Map.keys

{-|
Gets all params at range, for (:->:)
-}
getSecondParam::(Eq b) => Map.Map k (a, b) -> [b]
getSecondParam = nub . map snd . Map.elems

{-|
Gets all params at range, for (:-<:)
-}
getSecondParamSet::(Ord b) => Map.Map k (a, b) -> Set.Set b
getSecondParamSet = Set.fromList . map snd . Map.elems

{-|
Gets all states at domain, for (:->:)
-}
getStateDomain::(Eq a) => Map.Map (a, b) a1 -> [a]
getStateDomain = nub . map fst . Map.keys

{-|
Gets all states at domain, for (:-<:)
-}
getStateDomainSet::(Ord a) => Map.Map (a, b) a1 -> Set.Set a
getStateDomainSet = Set.fromList . map fst . Map.keys

{-|
Gets first param at range, for (:->:)
-}
getStateRange::(Eq a) => Map.Map k (a, b) -> [a]
getStateRange = nub . map fst . Map.elems

{-|
Gets first param at range, for (:-<:)
-}
getStateRangeSet::(Ord a) => Map.Map k (a, b) -> Set.Set a
getStateRangeSet = Set.fromList . map fst . Map.elems

{-|
Gets state at range in a list, for (:->:)
-}
getStateRangeD::(Eq a) => (:->:) a p1 p2 -> [Label a]
getStateRangeD = getStateRange

{-|
Gets state at range in a set, for (:->:)
-}
getStateRangeSetD::(Ord a) => (:->:) a p1 p2 -> Set.Set (Label a)
getStateRangeSetD = getStateRangeSet

{-|
Gets state at range in a list, for (:-<:)
-}
getStateRangeND::(Ord a) => (:-<:) a p1 p2 -> [Label a]
getStateRangeND = Set.toList . Set.unions . map fst . Map.elems

{-|
Gets state at range in a set, for (:->:) and (:-<:)
-}
getStateRangeSetND::(Ord a) => (:-<:) a p1 p2 -> Set.Set (Label a)
getStateRangeSetND = Set.unions . map fst . Map.elems
