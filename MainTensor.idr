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

main : IO ()
main = print $ runLayer (TN [TZ 1, TZ 2, TZ 3]) lyr
