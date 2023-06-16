open Core

let step x = if Float.(x > 0.) then 1. else 0.

let sigmoid x = Float.(1. / (1. + exp (-x)))

let relu x = Float.(max 0. x)
