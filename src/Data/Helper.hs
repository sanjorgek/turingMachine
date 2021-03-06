{-# OPTIONS_HADDOCK show-extensions #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-|
Module      : Helper
Description : Aux Functions
Copyright   : (c) Jorge Santiago Alvarez Cuadros, 2016
License     : GPL-3
Maintainer  : sanjorgek@ciencias.unam.mx
Stability   : experimental
Portability : portable

Auxiliar Functions
-}
module Data.Helper
(
  -- * Set
  unionsFold
  ,setGenericSize
) where
import qualified Data.Foldable as Fold
import           Data.List
import qualified Data.Map.Lazy as Map
import qualified Data.Set      as Set

{-|
Union of set monad
-}
unionsFold:: (Ord a, Fold.Foldable t) => t (Set.Set a) -> Set.Set a
unionsFold = Fold.foldr Set.union Set.empty

{-|
Size of a set, with large integers
-}
setGenericSize:: (Ord a) => Set.Set a -> Integer
setGenericSize s = if Set.null s
  then 0
  else 1 + setGenericSize (Set.delete (Set.findMin s) s)
