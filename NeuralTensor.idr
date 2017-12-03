module NeuralTensor

import Data.Vect
import Data.List
import Matrix
import DataTensor

%access public export

interface Layer (layer : Vect inr Nat -> Vect outr Nat -> Type) where
  runLayer :    Tensor inr i Double
             -> layer i o
             -> Tensor outr o Double

data FullyConnected : (inr : Nat) -> (outr : Nat) -> Vect inr Nat -> Vect outr Nat -> Type where
  MkFullyConnected :      (inr = 1)
                      ->  (outr = 1)
                      ->  (biases  : Tensor 1 o Double)
                      ->  (weights : Tensor 2 ((head o) :: i) Double)
                      ->  FullyConnected 1 1 i o

Layer (FullyConnected 1 1) where
  runLayer {i=[m]} {o=[n]} input (MkFullyConnected inr outr biases weights) = weights #* input #+ biases

Show (FullyConnected 1 1 i o) where
  show (MkFullyConnected inr outr biases weights) = "FullyConnected\n  " ++ (show biases) ++ "\n  " ++ (show weights)


data ReLU : Vect inr Nat -> Vect outr Nat -> Type where
  MkReLU : ReLU s s

Layer (ReLU) where
  runLayer input MkReLU = map (\e => if e > 0 then e else 0) input

Show (ReLU s s) where
  show MkReLU = "ReLU"


data Softmax : Vect inr Nat -> Vect outr Nat -> Type where
  MkSoftmax : Softmax s s

Layer (Softmax) where
  runLayer input MkSoftmax = map (/ s) input'
    where input' = map exp input
          s = sum input'

Show (Softmax s s) where
  show MkSoftmax = "Softmax"


data Logit : Vect inr Nat -> Vect outr Nat -> Type where
  MkLogit : Logit s s

Show (Logit s s) where
  show MkLogit = "Logit"
