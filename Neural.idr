module Neural

import Data.Vect
import Matrix

%access export

public export
interface Layer (layer : Nat -> Nat -> Type) where
  runLayer :    Vect  i   Double
             -> layer i o
             -> Vect    o Double

public export
data FullyConnected : Nat -> Nat -> Type where
  MkFullyConnected :    (biases  : Vect   o   Double)
                     -> (weights : Matrix o i Double)
                     -> FullyConnected i o

Show (FullyConnected i o) where
  show (MkFullyConnected biases weights) = "FullyConnected\n  " ++ (show biases) ++ "\n  " ++ (show weights)

public export
data ReLU : Nat -> Nat -> Type where
  MkReLU : ReLU s s

Show (ReLU s s) where
  show MkReLU = "ReLU"

public export
data Softmax : Nat -> Nat -> Type where
  MkSoftmax : Softmax s s

Show (Softmax s s) where
  show MkSoftmax = "Softmax"

public export
data Logit : Nat -> Nat -> Type where
  MkLogit : Logit s s

Show (Logit s s) where
  show MkLogit = "Logit"

-- infixr 5 :>:
--
-- data Network : Nat -> List Nat -> Nat -> Type -> Type where
--   Output : Layer   i           o a
--         -> Network i  []       o a
--   (:>:)   : Layer   i  h          a
--         -> Network    h    hs  o a
--         -> Network i (h :: hs) o a
--

Num a => Num (Vect n a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  fromInteger {n} = replicate n . fromInteger

interface Scalable a where
  scale : Double -> a -> a

Scalable Double where
  scale = (*)

Scalable a => Scalable (Vect n a) where
  scale lambda = map (scale lambda)

Neg a => Neg (Vect n a) where
  (-) = liftA2 (-)
  negate = map negate
  abs = map abs

Layer FullyConnected where
  runLayer input (MkFullyConnected biases weights) = weights .* input + biases

Layer ReLU where
  runLayer input MkReLU = map (\e => if e > 0 then e else 0) input

Layer Softmax where
  runLayer input MkSoftmax = map (/ s) input'
    where input' = map exp input
          s = sum input'

Layer Logit where
  runLayer input MkLogit = map logistic input
    where logistic x = 1 / (1 + exp (-x))

--
-- sigmoidD : Double -> Double
-- sigmoidD a = 1 / (1 + exp (-a))
--
-- sigmoidD' : Double -> Double
-- sigmoidD' a = let s = sigmoidD a
--               in s * (1 - s)
--
-- sigmoid : Vect n Double -> Vect n Double
-- sigmoid = map sigmoidD
--
-- sigmoid' : Vect n Double -> Vect n Double
-- sigmoid' = map sigmoidD'
--
-- calc : Vect     i Double
--     -> Vect   o   Double
--     -> Matrix o i Double
--     -> Vect   o   Double
-- calc input bias weights = weights .* input + bias
--
-- runLayer : Vect  i   Double
--         -> Layer i o Double
--         -> Vect    o Double
-- runLayer input (MkLayer bias weights) = calc input bias weights
--
-- runLayerS : Vect  i   Double
--          -> Layer i o Double
--          -> Vect    o Double
-- runLayerS input layer = sigmoid $ runLayer input layer
--
-- feedForward : Vect    i      Double
--            -> Network i hs o Double
--            -> Vect         o Double
-- feedForward input (l :>: ls) = let input' = (runLayerS input l)
--                               in feedForward input' ls
-- feedForward input (Output layer) = runLayerS input layer
--
-- outer : Num a
--      => Vect m a
--      -> Vect n a
--      -> Matrix m n a
-- outer vm vn = (transpose [vm]) #*# [vn]
--
-- predictionError : Vect    i      Double
--                -> Vect         o Double
--                -> Network i hs o Double
--                -> Vect         o Double
-- predictionError input target net = target - (feedForward input net)
--
-- backprop : Double
--         -> Vect    i      Double
--         -> Vect         o Double
--         -> Network i hs o Double
--         -> Network i hs o Double
-- backprop eta input target net = fst (go input target net)
--   where
--     go :  Vect    i      Double
--       ->  Vect         o Double
--       ->  Network i hs o Double
--       -> (Network i hs o Double, Vect i Double)
--     go input target (layer@(MkLayer bias weights) :>: rest) =
--       let y = runLayer input layer
--           output = sigmoid y
--
--           (rest', dWs') = go output target rest
--
--           dEdy = (sigmoid' y) * dWs'
--           --
--           bias' = bias - (eta `scale` dEdy)
--           weights' = weights - (eta `scale` (outer dEdy input))
--           layer' = (MkLayer bias' weights')
--
--           dWs = (transpose weights) .* dEdy
--       in (layer' :>: rest', dWs)
--
--     go input target (Output layer@(MkLayer bias weights)) =
--       let y = runLayer input layer
--           output = sigmoid y
--
--           dEdy = (sigmoid' y) * (output - target)
--           --
--           bias' = bias - (eta `scale` dEdy)
--           weights' = weights - (eta `scale` (outer dEdy input))
--           layer' = (MkLayer bias' weights')
--
--           dWs = (transpose weights) .* dEdy
--       in (Output layer', dWs)
--
-- initialNet : Network 2 [2] 2 Double
-- initialNet = first :>: second
--   where first  = MkLayer   [0.35, 0.35]
--                          [ [0.15, 0.20]
--                          , [0.25, 0.30]
--                          ]
--         second = Output
--                $ MkLayer   [0.60, 0.60]
--                          [ [0.40, 0.45]
--                          , [0.50, 0.55]
--                          ]
--
-- input : Vect 2 Double
-- input = [0.05,0.10]
--
-- -- should be "target", but meh
-- output : Vect 2 Double
-- output = [0.01,0.99]
--
