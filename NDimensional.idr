module NDimensional

import Data.Vect

data NDVect : (rank : Nat) -> (shape : Vect rank Nat) -> Type -> Type where
  NDVZ : (value : t) -> NDVect Z [] t
  NDV  : (values : Vect n (NDVect r s t)) -> NDVect (S r) (n::s) t

nmap : (t -> u) -> (NDVect r s t) -> NDVect r s u
nmap f (NDVZ value) = NDVZ (f value)
nmap f (NDV values) = NDV (map (nmap f) values)
