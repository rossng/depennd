module DataTensor

import Data.Vect
import Data.List
import Matrix

%access export

public export
data Tensor : (rank : Nat) -> (shape : Vect rank Nat) -> Type -> Type where
  TZ : (value : t) -> Tensor Z [] t
  TN : (values : Vect n (Tensor r s t)) -> Tensor (S r) (n::s) t

showImpl : Show t => Nat -> Tensor r s t -> String
showImpl d (TZ v) = show v
showImpl {s=(sh::shs)} d (TN vs) =
  indent ++ "[\n" ++ indent ++ " " ++ str ++ "\n" ++ indent ++ "]" where
                       elems : Vect sh String
                       elems = map (showImpl (S d)) vs
                       str : String
                       str = foldr (++) "" $ intersperse "," elems
                       indent : String
                       indent = pack (Data.Vect.replicate d ' ')

Show t => Show (Tensor r s t) where
  show (TZ value) = show value
  show tensor = showImpl 0 tensor

Functor (Tensor r s) where
  map f (TZ value)  = TZ (f value)
  map f (TN values) = TN (map (map f) values)

foldrImpl : (t -> acc -> acc) -> acc -> (acc -> acc) -> Tensor 1 n t -> acc
foldrImpl f e go (TN []) = go e
foldrImpl f e go (TN ((TZ x)::xs)) = assert_total $ DataTensor.foldrImpl f e (go . (f x)) (TN xs)

Foldable (Tensor 1 n) where
  foldr f e xs = DataTensor.foldrImpl f e id xs

-- TODO: generalise Foldable instance to arbitrary-rank Tensors

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

dot : Num t => Tensor 1 m t -> Tensor 1 m t -> t
dot a b = sum $ zipWith (*) a b

infixl 9 #*

(#*) : Num t => Tensor 2 [n,m] t -> Tensor 1 [m] t -> Tensor 1 [n] t
(TN mat) #* vec = TN $ map (TZ . dot vec) mat

ten3 : Tensor 3 [2,3,2] Integer
ten3 = TN [ TN [TN [TZ 1, TZ 1], TN [TZ 1, TZ 1], TN [TZ 1, TZ 1]]
          , TN [TN [TZ 1, TZ 1], TN [TZ 1, TZ 1], TN [TZ 1, TZ 1]]]

mat1 : Tensor 2 [2,3] Integer
mat1 = TN [ TN [TZ 1, TZ 2, TZ 3]
          , TN [TZ 1, TZ 1, TZ 1]]

vec1 : Tensor 1 [3] Integer
vec1 = TN [TZ 1, TZ 1, TZ 1]

mul1 : Tensor 1 [2] Integer
mul1 = mat1 #* vec1
