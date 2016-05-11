{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-|
Module      : Turing1T4W
Description : Four ways turing machine
Copyright   : (c) Jorge Santiago Alvarez Cuadros, 2016
License     : GPL-3
Maintainer  : sanjorgek@ciencias.unam.mx
Stability   : experimental
Portability : portable

Four ways turing machine
-}
module Math.Model.Turing.FourWays where
import Data.Delta
import Data.State
import Data.Sigma
import Math.Model.Turing
import Math.Model.Turing.TwoWays
import Data.List
import Data.Monoid
import qualified Data.Foldable as Fold

data Tracks a = Track [Tape a] (Tape a) [Tape a] deriving(Eq)

instance (Show a) => Show (Tracks a) where
	show (Track xts ts yts) = let 
			f x = "--"++(show x)++"\n"
			g x = "->"++(show x)++"\n"
			h x y = (concat.(map x)) y
		in (h f xts)++(g ts)++(h f yts)

instance Functor Tracks where
	fmap f (Track xts ts yts) = let 
			g = map (fmap f)
		in Track (g xts) (fmap f ts) (g yts)

instance Applicative Tracks where
	pure x = Track [] (pure x) []
	(<*>) (Track _ ft _) (Track _ t _) = Track [] (ft <*> t) []

instance (Eq s, Monoid s) => Monoid (Tracks s) where
	mempty = Track [] mempty []
	mappend (Track xts ts yts) (Track zts ss wts) = let 
			f = zipWith mappend
		in Track (f xts zts) (mappend ts ss) (f yts wts)

instance Tapeable Tracks Symbol where
	getHead (Track _ ts _) = getHead ts
	liftTape ws = Track [] (liftTape ws) []

instance TuringM Tape Symbol FW where 
	moveHead Rt (T xs a []) = T (xs++[a]) mempty []
	moveHead Rt (T xs a (y:ys)) = T (xs++[a]) y ys
	moveHead Lf (T [] a ys) = T [] mempty (a:ys)
	moveHead Lf (T xs a ys) = T (init xs) (last xs) (a:ys)

instance TuringM Tracks Symbol FW where
	moveHead Up (Track [] ts yts) = Track [] mempty (ts:yts)
	moveHead Up (Track xts ts yts) = Track (init xts) (last xts) (ts:yts)
	moveHead Dw (Track xts ts []) = Track (xts++[ts]) mempty []
	moveHead Dw (Track xts ts (ys:yts)) = Track (xts++[ts]) ys yts