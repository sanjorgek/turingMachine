{-# OPTIONS_HADDOCK show-extensions #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-|
Module      : Sigma
Description : Alphabet and symbols
Copyright   : (c) Jorge Santiago Alvarez Cuadros, 2016
License     : GPL-3
Maintainer  : sanjorgek@ciencias.unam.mx
Stability   : experimental
Portability : portable

Alphabet and symbols of languaje
-}
module Data.Sigma 
(
	Symbol(..)
	,blank
	,z0
	,Wd(..)
) where
import Data.Monoid
import Data.Char
import qualified Data.Set as Set

{-|
Symbols are character, and with Unicode CharSet we have a big amount of them.
-}
type Symbol = Char

{-|
Symbol type are forced to be a monoid
-}
instance Monoid Symbol where
	mempty = blank
	mappend x y = chr (mod (ord x + ord y) (ord maxBound))

-- |Blank symbol
blank::Symbol
blank = '\164'

-- |Initial symbol for stack
z0::Symbol
z0 = '\248'

{-|
List symbol alias, Word are defined in Prelude
-}
type Wd = [Symbol]

data Lang = Lang (Set.Set Symbol) [Wd]
