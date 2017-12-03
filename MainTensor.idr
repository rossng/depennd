module Main

import NeuralTensor
import DataTensor
import NetworkTensor
import Matrix
import Data.Vect

fc : FullyConnected 1 1 [3] [2]
fc = MkFullyConnected Refl Refl
                       (TN [TZ 0.1, TZ 0.1])
                       (TN [
                          TN [TZ 0.1, TZ 0.1, TZ 0.1]
                        , TN [TZ 0.1, TZ 0.1, TZ 0.1] ])

relu : ReLU [2] [2]
relu = MkReLU

softmax : Softmax [2] [2]
softmax = MkSoftmax

input : Tensor 1 [3] Double
input = TN [TZ 1, TZ 2, TZ 3]

negativeInput : Tensor 1 [3] Double
negativeInput = map (*(-1)) input

network : Network [3] [[2],[2]] [2]
network = (fc :>: relu :>: Output softmax)

main : IO ()
main = print $ runNetwork input network
