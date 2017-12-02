module Matrix

import Data.Vect

%access export

dot : Num a => Vect n a -> Vect n a -> a
dot va vb = foldr (+) 0 $ zipWith (*) va vb

public export
Matrix : (rows : Nat) -> (cols : Nat) -> Type -> Type
Matrix r c a = Vect r (Vect c a)

infixl 9 #*#

(#*#) : Num a => Matrix i j a -> Matrix j k a -> Matrix i k a
A #*# B = map (\Aj => map (dot Aj) (transpose B)) A
