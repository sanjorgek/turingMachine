{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-|
Module      : Turing1T4W
Description : Maquina de turing de cinta infinita
Copyright   : (c) Jorge Santiago Alvarez Cuadros, 2016
License     : GPL-3
Maintainer  : sanjorgek@ciencias.unam.mx
Stability   : experimental
Portability : portable

Maquina de turing de cinta infinita
-}
module Math.Model.Turing.TwoWays where
import Data.Delta
import Data.State
import Data.Sigma
import Math.Model.Turing
import Data.List
import Data.Monoid
import Control.Applicative
import qualified Data.Foldable as Fold

data Tape a = T [a] a [a] deriving(Show, Eq)

instance Functor Tape where
	fmap f (T xs a ys) = T (map f xs) (f a) (map f ys)

instance Applicative Tape where
	pure x = T [] x [] 
	--
	(<*>) (T fs f gs) (T xs a ys) = T [] (f a) []

instance (Eq s, Monoid s) => Monoid (Tape s) where
	mempty = T [] mempty []
	mappend (T xs a ys) (T [] b zs) = if 
			b==mempty 
		then T xs a (ys++zs) 
		else T xs a (ys++(b:zs))
	mappend t (T (x:xs) a ys) = if 
			x==mempty 
		then mappend t (T [] mempty (xs++(a:ys))) 
		else mappend t (T [] x (xs++(a:ys)))

{-|
>>> let tapeLifted = (liftTape "word")::Tape Symbol
>>> tapeLifted
T "" 'w' "ord"
-}
instance Tapeable Tape Symbol where
	getHead (T _ a _) = a
	liftTape ws = Fold.foldMap pure ws

instance Tapeable Tape [Symbol] where
	getHead (T _ as _) = as
	liftTape [] = T [[]] [blank] [[]]
	liftTape wss = let
			f = map head
			g = map tail
		in T (genericReplicate (genericLength wss) []) (f wss) (g wss)

instance TuringM Tape Symbol LRS where
	moveHead S t = t
	moveHead R (T xs a []) = T (xs++[a]) mempty []
	moveHead R (T xs a (y:ys)) = T (xs++[a]) y ys
	moveHead L (T [] a ys) = T [] mempty (a:ys)
	moveHead L (T xs a ys) = T (init xs) (last xs) (a:ys)

instance TuringM Tape [Symbol] LRS where
	moveHead S t = t
	moveHead R (T xss as []) = let
			f z = zipWith (\x y -> x++[y]) z
			g x = genericReplicate (genericLength x) mempty
		in T (f xss as) (g as) []
	moveHead R (T xss as l@([]:yss)) = let
			f z = zipWith (\x y -> x++[y]) z
			g x = genericReplicate (genericLength x) mempty
		in T (f xss as) (g as) l
	moveHead R (T xss as yss) = let
			f = map head
			g = map tail
			h z = zipWith (\x y -> x++[y]) z
		in T (h xss as) (f yss) (g yss)
	moveHead L (T [] as yss) = let
			g x = genericReplicate (genericLength x) mempty
			f x y = zipWith (:) x y
		in T [] (g as) (f as yss)
	moveHead L (T l@([]:xss) as yss) = let
			f x y = zipWith (:) x y
			g x = genericReplicate (genericLength x) mempty
		in T l (g as) (f as yss)
	moveHead L (T xss as yss) = let
			f = map last
			g = map init
			h z = zipWith (:) z
		in T (g yss) (f yss) (h as xss)