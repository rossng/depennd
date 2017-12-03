module NeuralTensor

import Data.Vect
import Data.List
import Matrix
import DataTensor

%access export

public export
interface Layer (layer : Vect inr Nat -> Vect outr Nat -> Type) where
  runLayer :    Tensor inr i Double
             -> layer i o
             -> Tensor outr o Double

public export
data FullyConnected : (inr : Nat) -> (outr : Nat) -> Vect inr Nat -> Vect outr Nat -> Type where
  MkFullyConnected :      (inr = 1)
                      ->  (outr = 1)
                      ->  (biases  : Tensor 1 o Double)
                      ->  (weights : Tensor 2 ((head o) :: i) Double)
                      ->  FullyConnected 1 1 i o

public export
Layer (FullyConnected 1 1) where
  runLayer {i=[m]} {o=[n]} input (MkFullyConnected inr outr biases weights) = weights #* input #+ biases
