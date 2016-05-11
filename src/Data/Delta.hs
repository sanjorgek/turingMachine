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
	-- ** Determinist
	-- *** Constructor
	(:->:)(..)
	-- *** Functions
	,nextD
	-- ** Not determinist
	-- *** Constructor
	,(:>-:)(..)
	-- * Transductor
	-- ** Constructor
	,(:*>:)(..)
) where
import Data.State
import Control.Applicative
import Data.Monoid
import Data.Foldable
import qualified Data.Map.Lazy as Map

type (:->:) a p1 p2 = Map.Map (State a, p1) (State a, p2)

nextD :: (Ord p1, Ord a) => (:->:) a p1 p2 -> (State a, p1) -> State a
nextD dt k = if Map.member k dt then fst (dt Map.! k) else QE 

type (:>-:) a p1 p2 = Map.Map (State a, p1) ([State a], p2)

type (:*>:) a p o = Map.Map (State a, p) o