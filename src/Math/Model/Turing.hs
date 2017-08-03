{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTSyntax                #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE TypeOperators             #-}
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
import           Control.Applicative
import           Data.Delta
import qualified Data.Foldable       as Fold
import           Data.Label
import           Data.List
import qualified Data.Map.Strict     as Map
import           Data.Monoid
import           Data.Sigma

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

newtype MultiTape t a = MT [t a]

getMHead::(Tapeable t a) => MultiTape t a -> [a]
getMHead (MT ts) = [getHead t | t<-ts]

liftMTape:: (Tapeable t a, Monoid (t a)) => [a] -> MultiTape t a
liftMTape ws = MT [liftTape ws]

class (Tapeable t b, Ways w) => TuringM t b w where
	moveHead::(Monoid b) => w -> t b -> t b

data Model a b c where
	TS::(Ways c) => Delta a b c->Label a->Final a->Model a b c

data MultiModel a b c where
	MTS::(Ways c) => MDelta a b c->Label a->[Final a]->MultiModel a b c
