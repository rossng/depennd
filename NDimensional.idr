module NDimensional

import Data.Vect
import Data.List
import Matrix

data NDVect : (shape : List Nat) -> Type -> Type where
  NDVZ : (value : t) -> NDVect [] t
  NDV  : (values : Vect n (NDVect s t)) -> NDVect (n::s) t

nmap : (t -> u) -> (NDVect s t) -> NDVect s u
nmap f (NDVZ value) = NDVZ (f value)
nmap f (NDV values) = NDV (map (nmap f) values)

-- interface Layer (layer : List Nat -> List Nat -> Type) where
--   runLayer :    Vect  i   Double
--              -> layer i o
--              -> Vect    o Double
--
-- data FullyConnected : Nat -> Nat -> Type where
--   MkFullyConnected :    (biases  : Vect   o   Double)
--                      -> (weights : Matrix o i Double)
--                      -> FullyConnected i o
