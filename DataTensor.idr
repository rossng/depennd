module DataTensor

import Data.Vect
import Data.List
import Matrix

data Tensor : (rank : Nat) -> (shape : Vect rank Nat) -> Type -> Type where
  TZ : (value : t) -> Tensor Z [] t
  TN : (values : Vect n (Tensor r s t)) -> Tensor (S r) (n::s) t

Functor (Tensor r s) where
  map f (TZ value)  = TZ (f value)
  map f (TN values) = TN (map (map f) values)

infixr 7 ::

(::) : Tensor r shs t -> Tensor (S r) (sh::shs) t -> Tensor (S r) ((S sh)::shs) t
(::) (TZ x) (TN xs) = TN (TZ x :: xs)
(::) (TN x) (TN xs) = TN (TN x :: xs)

zipWith : {r : Nat} -> (f : a -> b -> c) -> (Tensor r s a) -> (Tensor r s b) -> (Tensor r s c)
zipWith {r=Z} {s=[]} f (TZ x) (TZ y) = TZ (f x y)
zipWith {r=(S k)} {s=(Z::shs)} f (TN []) (TN []) = TN []
zipWith {r=(S k)} {s=((S sh)::shs)} f (TN (x::xs)) (TN (y::ys)) = hd :: tl where
  hd : Tensor k shs c
  hd = (zipWith f x y)
  tl : Tensor (S k) (sh::shs) c
  tl = zipWith f (TN xs) (TN ys)

infixl 9 #+

(#+) : Num t => Tensor r s t -> Tensor r s t -> Tensor r s t
a #+ b = zipWith (+) a b

-- interface Layer (layer : List Nat -> List Nat -> Type) where
--   runLayer :    Vect  i   Double
--              -> layer i o
--              -> Vect    o Double
--
-- data FullyConnected : List Nat -> List Nat -> Type where
--   MkFullyConnected :    {i: []}
--                      -> (biases  : Vect   o   Double)
--                      -> (weights : Matrix o i Double)
--                      -> FullyConnected i o
