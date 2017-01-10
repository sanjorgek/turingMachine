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
	,nextD
	-- ** Not deterministic
	-- *** Constructor
	,(:-<:)(..)
  -- *** Functions
  ,nextND
	-- * Transductor
	-- ** Constructor
	,(:*>:)(..)
  -- ** Functions
  ,nextT
  -- * Auxiliar functions
  ,getFirstParam
  ,getSecondParam
  ,getStateDomain
  ,getStateRange
) where
import           Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import           Data.Sigma
import           Data.State

{-|
Deterministic Delta

Maps a tuple, a state and a param, to a tuple, a state and a param.
-}
type (:->:) a p1 p2 = Map.Map (State a, p1) (State a, p2)

{-|
Next state function for deterministic delta
-}
nextD :: (Ord p1, Ord a) => (:->:) a p1 p2 -> (State a, p1) -> State a
nextD dt k = if Map.member k dt then fst (dt Map.! k) else QE

{-|
Non-Deterministic Delta

Maps a tuple, a state and a param, to a tuple, a state list and a param.
-}
type (:-<:) a p1 p2 = Map.Map (State a, p1) (Set.Set (State a), p2)

{-|
Next state function for non-deterministic delta
-}
nextND :: (Ord p1, Ord a) => (:-<:) a p1 p2 -> (State a, p1) -> Set.Set (State a)
nextND dt k = if Map.member k dt then fst (dt Map.! k) else Set.insert QE Set.empty

{-|
Map a tuple, a state and a param, to some output
-}
type (:*>:) a p o = Map.Map (State a, p) o

{-|
For simple map with Chars range
-}
nextT :: (Ord p1, Ord a) => (:*>:) a p1 Symbol -> (State a, p1) -> Symbol
nextT dt k = if Map.member k dt then dt Map.! k else '\0'

{-|
Gets all params at domain, for (:->:) and (:-<:)
-}
getFirstParam::(Eq b) => Map.Map (a, b) a1 -> [b]
getFirstParam = nub . snd . unzip . Map.keys

{-|
Gets all params at range, for (:->:) and (:-<:)
-}
getSecondParam::(Eq b) => Map.Map k (a, b) -> [b]
getSecondParam = nub . snd . unzip . Map.elems

{-|
Gets all states at domain, for (:->:) and (:-<:)
-}
getStateDomain::(Eq a) => Map.Map (a, b) a1 -> [a]
getStateDomain = nub . fst . unzip . Map.keys

{-|
Gets all params at range, for (:->:) and (:-<:)
-}
getStateRange::(Eq a) => Map.Map k (a, b) -> [a]
getStateRange = nub . fst . unzip . Map.elems
