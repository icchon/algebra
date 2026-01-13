module type MIN = sig
  type t

  val one : t
  val mul : t -> t -> t
  val equal : t -> t -> bool
end

module type S = sig
  include MIN
  include Atom.S with type t := t

  val is_one : t -> bool
  val pow : t -> int -> t
  val pow_non_neg : t -> int -> t
end

module Extend (M : MIN) = struct
  let is_one x = M.equal x M.one

  let pow_non_neg x n =
    let rec aux acc n = if n = 0 then acc else aux (M.mul acc x) (n - 1) in
    aux M.one n

  let pow = pow_non_neg
end
