module Extend
    (Scalar : Division_ring.S)
    (Act : sig
      type e

      val scale : e -> Scalar.t -> e
    end)
    (V : Traversable.S) =
struct
  let div_scale v s =
    let s_inv = Scalar.inv s in
    V.map (fun e -> Act.scale e s_inv) v
end

module type S = sig
  type s
  type t

  val div_scale : t -> s -> t
end
