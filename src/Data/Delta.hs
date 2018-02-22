{-# OPTIONS_GHC -fno-warn-tabs      #-}
{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE TypeOperators          #-}
{-|
Module      : Data.Delta
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
	,nextOutput
  ,nextSymbol
  -- * Auxiliar functions
	,getStateDomain
	,getStateDomainSet
  ,getParamDomain
  ,getParamDomainSet
	,getRange
	,getRangeSet
	,getStateRangeD
	,getStateRangeSetD
	,getStateRangeND
	,getStateRangeSetND
	,getSecondParamD
  ,getSecondParamND
	,getSecondParamSetD
  ,getSecondParamSetND
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
  in Map.fromList $ zip (zip (fmap return xs) ys) zs

{-|
Take a state and a param and maybe resolve some output
-}
nextTMaybe :: (Ord p1, Ord a) => (:*>:) a p1 o -> (Label a, p1) -> Maybe o
nextTMaybe dt k = if Map.member k dt
  then Just $ dt Map.! k
  else Nothing

{-|
Take a state and a parem and retrun a output, or a default value
-}
nextOutput::(Ord p1, Ord a) => (:*>:) a p1 o -> o -> (Label a, p1) -> o
nextOutput dt o k = fromMaybe o $ nextTMaybe dt k

{-|
For simple map with Chars range
-}
nextSymbol::(Ord p1, Ord a) => (:*>:) a p1 Symbol -> (Label a, p1) -> Symbol
nextSymbol dt = nextOutput dt '\NUL'

{-|
Deterministic Delta with one param

Maps a tuple, a state and a param, to another tuple, a state and a param.
-}
type (:->:) a p1 p2 = (:*>:) a p1 (Label a, p2)

{-|
Lifts a deterministic delta from a 4-tuple list
-}
liftD::(Ord a, Ord p1) => [(a, p1, a, p2)] -> (:->:) a p1 p2
liftD ds = let
    (xs, ys, ws, zs) = unzip4 ds
  in liftL $ zip3 xs ys $ zip (fmap return ws) zs

{-|
Next state function for deterministic delta
-}
nextD :: (Ord p1, Ord a) => (:->:) a p1 p2 -> (Label a, p1) -> Label a
nextD dt k = maybe QE fst $ nextTMaybe dt k

{-|
Non-Deterministic Delta

Maps a tuple, a state and a param, to a tuple, a state list and a param.
-}
type (:-<:) a p1 p2 = (:*>:) a p1 (Set.Set (Label a, p2))

{-|
Lifts a non-deterministic delta from a 4-tuple list
-}
liftND::(Ord a, Ord p1, Ord p2) => [(a, p1, [(a,p2)])] -> (:-<:) a p1 p2
liftND ds = let
		(xs, ys, wss) = unzip3 ds
		f (x,y) = (return x, y)
	in liftL $ zip3 xs ys $ fmap (Set.fromList . fmap f) wss

{-|
Next state function for non-deterministic delta
-}
nextND :: (Ord p1, Ord a) => (:-<:) a p1 p2 -> p2 -> (Label a, p1) -> Set.Set (Label a)
nextND dt p k =  maybe (Set.singleton QE) (Set.map fst) $ nextTMaybe dt k

{-|
Gets all params at domain, for all (:*>:)
-}
getParamDomain::(Eq b) => (:*>:) a b o -> [b]
getParamDomain = nub . fmap snd . Map.keys

{-|
Gets all params at domain, for all (:*>:)
-}
getParamDomainSet::(Ord b) => (:*>:) a b o -> Set.Set b
getParamDomainSet = Set.fromList . fmap snd . Map.keys

{-|
Gets all states at domain, for all (:*>:)
-}
getStateDomain::(Eq a) => (:*>:) a b o -> [Label a]
getStateDomain = nub . fmap fst . Map.keys

{-|
Gets all states at domain, for all (:*>:)
-}
getStateDomainSet::(Ord a) => (:*>:) a b o -> Set.Set (Label a)
getStateDomainSet = Set.fromList . fmap fst . Map.keys

{-|
Gets param at range, for (:*>:)
-}
getRange::(Eq o) => (:*>:) a b o -> [o]
getRange = nub . Map.elems

{-|
Gets param at range, for (:*>:)
-}
getRangeSet::(Ord o) => (:*>:) a b o -> Set.Set o
getRangeSet = Set.fromList . Map.elems

{-|
Gets all params at range, for (:->:)
-}
getSecondParamD::(Eq p2) => (:->:) a p1 p2 -> [p2]
getSecondParamD = nub . fmap snd . Map.elems

{-|
Gets all params at range, for (:->:)
-}
getSecondParamSetD::(Ord b) => Map.Map k (a, b) -> Set.Set b
getSecondParamSetD = Set.fromList . fmap snd . Map.elems

{-|
Gets all params at range, for (:-<:)
-}
getSecondParamND::(Ord p2) => (:-<:) a p1 p2 -> [p2]
getSecondParamND = foldr union [] . fmap (Set.toList . Set.map snd) . Map.elems

{-|
Gets all params at range, for (:-<:)
-}
getSecondParamSetND::(Ord p2) => (:-<:) a p1 p2 -> Set.Set p2
getSecondParamSetND = Set.unions . fmap (Set.map snd) . Map.elems

{-|
Gets first param at range, for (:->:)
-}
getStateRangeD::(Eq a) => (:->:) a p1 p2 -> [Label a]
getStateRangeD = nub . fmap fst . Map.elems

{-|
Gets first param at range, for (:->:)
-}
getStateRangeSetD::(Ord a) => (:->:) a p1 p2 -> Set.Set (Label a)
getStateRangeSetD = Set.fromList . fmap fst . Map.elems

{-|
Gets state at range in a list, for (:-<:)
-}
getStateRangeND::(Ord a) => (:-<:) a p1 p2 -> [Label a]
getStateRangeND = Set.toList . Set.unions . fmap (Set.map fst) . Map.elems

{-|
Gets state at range in a set, for (:-<:)
-}
getStateRangeSetND::(Ord a) => (:-<:) a p1 p2 -> Set.Set (Label a)
getStateRangeSetND = Set.unions . fmap (Set.map fst)  . Map.elems
