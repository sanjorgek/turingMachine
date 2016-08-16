module Data.Tape where
import qualified Data.Foldable as Fold

class Tapeable t where
  getHead:: t a -> a
  liftTape :: (Monoid (t a)) => [a] -> t a

data Track a = Track [a] a [a] deriving(Show, Eq)

instance Functor Track where
  fmap f (Track xs h ys) = Track (map f xs) (f h) (map f ys)

zipFunc::[t1 -> t] -> [t1] -> [t]
zipFunc [] _ = []
zipFunc _ [] = []
zipFunc (f:fs) (x:xs) = f x : zipFunc fs xs

instance Applicative Track where
  pure x = Track [] x []
  (<*>) (Track fs f gs) (Track xs a ys) = Track (zipFunc fs xs) (f a) (zipFunc gs ys)

instance (Eq s, Monoid s) => Monoid (Track s) where
  mempty = Track [] mempty []
  mappend (Track xs a ys) (Track [] b zs) = Track xs a ((++) ys (b : zs))--(if b == mempty then zs else b : zs))
  mappend t (Track (x:xs) a ys) = mappend t (Track [] x (xs++(a:ys)))

instance Tapeable Track where
  getHead (Track _ h _) = h
  liftTape = Fold.foldMap pure
