module Tensor

import Data.Fin
import Data.Vect
import Matrix

tensor : Vect n Nat -> Type -> Type
tensor []        a = a
tensor (m :: ms) a = Vect m (tensor ms a)

tmap : (t -> u) -> tensor s t -> tensor s u
tmap {s=[]} f v = f v
tmap {s=(x::xs)} f vs = tmap f vs

-- tzipWith : (f : a -> b -> c) -> (tensor s a) -> (tensor s b) -> tensor s c
-- tzipWith {s=[]} f x y = f x y
-- tzipWith {s=[l]} f x y = zipWith f x y
-- tzipWith {s=(l::ls)} f (x::xs) y = ?hole
-- (tzipWith {s=ls} f x y) :: (tzipWith {s=((l-1)::ls)} f xs ys)

infixl 9 #*

(#*) : Num a => tensor [y,x] a -> tensor [x] a -> tensor [y] a
mat #* vec = map (dot vec) mat

infixl 9 #+

(#+) : Num a => tensor s a -> tensor s a -> tensor s a
(#+) {s=[]} t1 t2 = t1 + t2
(#+) {s=(x::xs)} (t::ts) t2 = ?hole

data Index : Vect n Nat -> Type where
  Here : Index []
  At   : Fin m -> Index ms -> Index (m :: ms)

index : Index ms -> tensor ms a -> a
index Here     a = a
index (At k i) v = index i $ index k v

interface Layer (layer : Vect n Nat -> Vect n Nat -> Type) where
  runLayer :    tensor i   Double
             -> layer  i o
             -> tensor   o Double
--
data FullyConnected : Vect n Nat -> Vect n Nat -> Type where
  MkFullyConnected :    {i: Vect 1 Nat}
                     -> {o: Vect 1 Nat}
                     -> (biases  : tensor o Double)
                     -> (weights : tensor (head o :: i) Double)
                     -> FullyConnected i o

Layer FullyConnected where
 runLayer input (MkFullyConnected {i=[x]} {o=[y]} biases weights) = weights #* input

lyr : FullyConnected [2] [3]
lyr = MkFullyConnected {i=[2]} {o=[3]} [1,2,3] [[1,1],[1,1],[1,1]]
