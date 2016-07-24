{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE TypeOperators #-}
{-|
Module      : Delta
Description : Implementacion de un mapeo
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
	,(:>-:)(..)
  -- *** Functions
  ,nextND
	-- * Transductor
	-- ** Constructor
	,(:*>:)(..)
) where
import Data.State
import Control.Applicative
import Data.Monoid
import Data.Foldable
import qualified Data.Map.Lazy as Map

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
type (:>-:) a p1 p2 = Map.Map (State a, p1) ([State a], p2)

{-|
Next state function for non-deterministic delta
-}
nextND :: (Ord p1, Ord a) => (:>-:) a p1 p2 -> (State a, p1) -> [State a]
nextND dt k = if Map.member k dt then fst (dt Map.! k) else [QE] 

{-|
Map a tuple, a state and a param, to some output
-}
type (:*>:) a p o = Map.Map (State a, p) o
