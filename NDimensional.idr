module NDimensional

import Data.Vect
import Matrix

data NDVect : (shape : Vect Nat Nat) -> Type -> Type where
  NDVZ : (value : t) -> NDVect [] t
  NDV  : (values : Vect n (NDVect s t)) -> NDVect (n::s) t

nmap : (t -> u) -> (NDVect s t) -> NDVect s u
nmap f (NDVZ value) = NDVZ (f value)
nmap f (NDV values) = NDV (map (nmap f) values)

-- interface Layer (layer : Nat -> Nat -> Type) where
--   runLayer :    Vect  i   Double
--              -> layer i o
--              -> Vect    o Double
