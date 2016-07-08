{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_HADDOCK show-extensions #-}
{-|
Module      : State
Description : Simple state data
Copyright   : (c) Jorge Santiago Alvarez Cuadros, 2016
License     : GPL-3
Maintainer  : sanjorgek@ciencias.unam.mx
Stability   : experimental
Portability : portable

Simple State function, have an isomorphism with Maybe but order are diferent
-}
module Data.State 
(
	-- * Data and type
	State(..)
	,Final(..)
	,terminal
	-- * Functions
	,isError
) where
import Control.Applicative
import Control.Monad
import Data.Monoid
import qualified Data.Foldable as F

{-|
Macine states are only a label, maybe a letter
-}
data State a = 
	-- |State constructor
	Q a 
	-- |Error state
	| QE deriving(Show, Eq, Ord)

-- |Same as Maybe
instance Functor State where
	fmap _ QE = QE
	fmap f (Q q) = Q $ f q 

-- |Same as Maybe
instance Applicative State where
	pure = Q
	QE <*> _ = QE
	(Q f) <*> q = fmap f q

-- |Same as Maybe
instance Monad State where
	return = pure
	QE >>= _ = QE
	(Q q) >>= f = f q

-- |Same as Maybe
instance (Enum a) => Enum (State a) where
	toEnum n = if n<0 then QE else Q (toEnum n)
	fromEnum QE = -1
	fromEnum (Q a) = fromEnum a

-- |In this differ with Maybe because this show a upper bounded order
instance (Bounded a)=> Bounded (State a) where
	minBound = Q minBound
	maxBound = QE 

instance Monoid a => Monoid (State a) where
	mempty = QE
	QE `mappend` m = m
	m `mappend` QE = m
	(Q a) `mappend` (Q b) = Q (a `mappend` b)

instance F.Foldable State where
    foldr _ z QE = z
    foldr f z (Q x) = f x z
    foldl _ z QE = z
    foldl f z (Q x) = f z x

{-|
Final state represent a set of states which elements put end to computation
-}
type Final a = [State a]

{-|
Tells if a state is final
-}
terminal :: (Eq a) => Final a -> State a -> Bool
terminal qs q = q `elem` qs

{-|
Tells if a state is a error state
-}
isError::(Eq a) => State a -> Bool
isError q = q==QE