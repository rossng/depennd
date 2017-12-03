module NetworkTensor

import Data.Vect
import Data.List
import Matrix
import DataTensor
import NeuralTensor

%access export

infixr 5 :>:

public export
data Network : (i : Vect inr Nat) -> List (Vect hidr Nat)
            -> (o : Vect outr Nat) -> Type where
  Output : Layer l => l i o -> Network i [] o
  (:>:)  : Layer l => l i h -> Network h hs o -> Network i (h::hs) o

runNetwork : Tensor inr i Double -> Network i hs o -> Tensor outr o Double
runNetwork input (Output layer) = runLayer input layer
runNetwork input (l :>: ls) = runNetwork (runLayer input l) ls
