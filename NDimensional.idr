module NDimensional

import Data.Vect
import Data.List
import Matrix

data NDVect : (rank : Nat) -> (shape : Vect rank Nat) -> Type -> Type where
  NDVZ : (value : t) -> NDVect Z [] t
  NDV  : (values : Vect n (NDVect r s t)) -> NDVect (S r) (n::s) t

nmap : (t -> u) -> (NDVect r s t) -> NDVect r s u
nmap f (NDVZ value) = NDVZ (f value)
nmap f (NDV values) = NDV (map (nmap f) values)

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
