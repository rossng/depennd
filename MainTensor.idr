module Main

import NeuralTensor
import DataTensor
import Matrix
import Data.Vect

lyr : FullyConnected 1 1 [3] [2]
lyr = MkFullyConnected Refl Refl
                       (TN [TZ 0.1, TZ 0.1])
                       (TN [
                          TN [TZ 0.1, TZ 0.1, TZ 0.1]
                        , TN [TZ 0.1, TZ 0.1, TZ 0.1] ])

lyr2 : ReLU [2] [2]
lyr2 = MkReLU

input : Tensor 1 [3] Double
input = TN [TZ 1, TZ 2, TZ 3]

negativeInput : Tensor 1 [3] Double
negativeInput = map (*(-1)) input

main : IO ()
main = print $ runLayer (runLayer negativeInput lyr) lyr2
