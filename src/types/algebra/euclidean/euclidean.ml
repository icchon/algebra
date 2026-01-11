module type MIN = sig
  type t

  include Commutable.S

  val zero : t
  val one : t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val quo : t -> t -> t
  val rem : t -> t -> t
end

module type S = sig
  include MIN
  include Add_group.S with type t := t

  val gcd_ext : t -> t -> t * t * t
  val gcd : t -> t -> t
end

module Extend (M : MIN) = struct
  let rec gcd_ext a b =
    if b = M.zero then (a, M.one, M.zero)
    else
      let g, x', y' = gcd_ext b (M.rem a b) in
      let q = M.quo a b in
      (g, y', M.sub x' (M.mul q y'))

  let gcd a b =
    let g, _, _ = gcd_ext a b in
    g
end
