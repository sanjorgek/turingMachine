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
module Data.Label
(
	-- * Data and type
	Label(..)
	,Final(..)
	-- * Functions
	,isError
	,terminal
  -- * Alias
  ,SetLabel(..)
  ,LabelSS(..)
) where
import           Control.Applicative
import           Control.Monad
import qualified Data.Foldable       as F
import           Data.Monoid
import qualified Data.Set            as Set

{-|
Macine states are only a label, maybe a letter
-}
data Label a =
	-- |State constructor
	Q a
	-- |Error state
	| QE deriving(Show, Eq)

-- |Same as Maybe
instance Functor Label where
	fmap _ QE = QE
	fmap f (Q q) = Q $ f q

-- |Same as Maybe
instance Applicative Label where
	pure = Q
	QE <*> _ = QE
	(Q f) <*> q = fmap f q

-- |Same as Maybe
instance Monad Label where
	return = pure
	QE >>= _ = QE
	(Q q) >>= f = f q

{-|
Holds

>>> QE /= (toEnum:: State Int) . fromEnum QE
True
-}
instance (Enum a) => Enum (Label a) where
  toEnum = return . toEnum
  fromEnum (Q x) = fromEnum x
  fromEnum QE    = maxBound

-- |In this differ with Maybe because this show a upper bounded order
instance (Bounded a) => Bounded (Label a) where
	minBound = Q minBound
	maxBound = QE

instance (Ord a) => Ord (Label a) where
  compare QE QE       = EQ
  compare _ QE        = LT
  compare QE _        = GT
  compare (Q a) (Q b) = compare a b

instance Monoid a => Monoid (Label a) where
	mempty = QE
	QE `mappend` m = m
	m `mappend` QE = m
	(Q a) `mappend` (Q b) = Q (a `mappend` b)

instance F.Foldable Label where
    foldr _ z QE    = z
    foldr f z (Q x) = f x z
    foldl _ z QE    = z
    foldl f z (Q x) = f z x

{-|
Final state represent a set of states which elements put end to computation
-}
type Final a = Set.Set (Label a)

{-|
Tells if a state is final
-}
terminal :: (Ord a) => Final a -> Label a -> Bool
terminal qs q = Set.member q qs

{-|
Tells if a state is a error state
-}
isError::(Eq a) => Label a -> Bool
isError = (QE==)

{-|
Alias for a set of states
-}
type SetLabel a = Set.Set (Label a)

{-|
Alias for a state of a set of states
-}
type LabelSS a = Label (SetLabel a)
