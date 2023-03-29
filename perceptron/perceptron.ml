open Core

let binary_gate w b x1 x2 =
  let x = [ float x1; float x2 ] in
  let tmp = List.fold2_exn ~init:0.0 x w ~f:Float.(fun acc x w -> acc + x * w) +. b in
  if Float.(tmp <= 0.0) then 0 else 1

let and_ = binary_gate [0.5; 0.5] (-0.7)
let%test _ = and_ 0 0 = 0
let%test _ = and_ 1 0 = 0
let%test _ = and_ 0 1 = 0
let%test _ = and_ 1 1 = 1

let nand = binary_gate [-0.5; -0.5] 0.7
let%test _ = nand 0 0 = 1
let%test _ = nand 1 0 = 1
let%test _ = nand 0 1 = 1
let%test _ = nand 1 1 = 0

let or_ = binary_gate [0.5; 0.5] (-0.2)
let%test _ = or_ 0 0 = 0
let%test _ = or_ 1 0 = 1
let%test _ = or_ 0 1 = 1
let%test _ = or_ 1 1 = 1

let ($|) f g x1 x2 = Tuple2.uncurry f @@ g x1 x2
let( *|) f g x1 x2 = f x1 x2, g x1 x2

let xor = and_ $| or_ *| nand
let%test _ = xor 0 0 = 0
let%test _ = xor 1 0 = 1
let%test _ = xor 0 1 = 1
let%test _ = xor 1 1 = 0


