module Main

import Neural
import Matrix
import Data.Vect

lyr : FullyConnected 3 2
lyr = MkFullyConnected   [0.1, 0.1]
                       [ [0.1, 0.1, 0.1]
                       , [0.1, 0.1, 0.1] ]

lyr2 : ReLU 2 2
lyr2 = MkReLU

main : IO ()
main = print $ runLayer (runLayer [1,2,3] lyr) lyr2
