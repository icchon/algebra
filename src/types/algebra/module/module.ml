module Extend
    (Act : sig
      type s
      type e

      val scale : e -> s -> e
    end)
    (V : Traversable.S) =
struct
  let mul_scale v s = V.map (fun e -> Act.scale e s) v
end

module type S = sig
  type s
  type t

  val mul_scale : t -> s -> t
end
