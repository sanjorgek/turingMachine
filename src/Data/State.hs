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
	-- * Functions
	,isError
	,terminal
) where
import           Control.Applicative
import           Control.Monad
import qualified Data.Foldable       as F
import           Data.Monoid
import qualified Data.Set            as Set

{-|
Macine states are only a label, maybe a letter
-}
data State a =
	-- |State constructor
	Q a
	-- |Error state
	| QE deriving(Show, Eq)

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

{-|
Holds

>>> QE /= (toEnum:: State Int) . fromEnum QE
True
-}
instance (Enum a) => Enum (State a) where
  toEnum = return . toEnum
  fromEnum (Q x) = fromEnum x
  fromEnum QE = maxBound

-- |In this differ with Maybe because this show a upper bounded order
instance (Bounded a) => Bounded (State a) where
	minBound = Q minBound
	maxBound = QE

instance (Ord a) => Ord (State a) where
  compare QE QE = EQ
  compare _ QE = LT
  compare QE _ = GT
  compare (Q a) (Q b) = compare a b

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
type Final a = Set.Set (State a)

{-|
Tells if a state is final
-}
terminal :: (Ord a) => Final a -> State a -> Bool
terminal qs q = Set.member q qs

{-|
Tells if a state is a error state
-}
isError::(Eq a) => State a -> Bool
isError = (QE==)
