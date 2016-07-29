{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE ExistentialQuantification #-}
{-|
Module      : Turing
Description : Turing machine abstaction
Copyright   : (c) Jorge Santiago Alvarez Cuadros, 2016
License     : GPL-3
Maintainer  : sanjorgek@ciencias.unam.mx
Stability   : experimental
Portability : portable

Turing machine abstaction
-}
module Math.Model.Turing where
import Data.Delta
import Data.State
import Data.Sigma
import Data.List
import Data.Monoid
import Control.Applicative
import qualified Data.Map.Strict as Map
import qualified Data.Foldable as Fold

class Ways a where
	oposite::a -> a

data LRS = 
	-- |Left move
	L
	-- |No move
	| S
	-- |Right move
	| R deriving(Show, Eq, Ord, Bounded)

instance Ways LRS where
	oposite L = R
	oposite R = L
	oposite S = S

data FW = 
	Dw
	|Lf
	|Rt
	|Up deriving(Show, Eq, Bounded)

instance Ways FW where
	oposite Up = Dw
	oposite Dw = Up
	oposite Lf = Rt
	oposite Rt = Lf

type Delta a b c= (:->:) a b (b,c)

type MDelta a b c = (:->:) a [b] ([b],[c])

liftD::(Ord a, Ord b) => [(a,b,a,b,c)]->Delta a b c
liftD = liftDAux

liftMD::(Ord a, Ord b) => [(a,[b],a,[b],[c])]->MDelta a b c
liftMD = liftDAux
	
liftDAux:: (Ord a, Ord b) => [(a,b,a,b,c)]-> (:->:) a b (b,c)
liftDAux ls = let
		(as,bs,cs,ds,es) = unzip5 ls
		f = map return
		xs = zip (f as) bs
		ys = zip (f cs) (zip ds es)
	in Map.fromList (zip xs ys)

class (Applicative t) => Tapeable t a where
	getHead::t a -> a
	liftTape::(Monoid (t a)) => [a] -> t a

data MultiTape t a = MT [t a] deriving(Show, Eq)

getMHead::(Tapeable t a) => MultiTape t a -> [a]
getMHead (MT ts) = [getHead t | t<-ts]

liftMTape:: (Tapeable t a, Monoid (t a)) => [a] -> MultiTape t a
liftMTape ws = MT [liftTape ws]

class (Tapeable t b, Ways w) => TuringM t b w where
	moveHead::(Monoid b) => w -> t b -> t b

data Model a b c where
	TS::(Ways c) => Delta a b c->State a->Final a->Model a b c

data MultiModel a b c where
	MTS::(Ways c) => MDelta a b c->State a->[Final a]->MultiModel a b c