module type MIN = sig
  type t

  val zero : t
  val add : t -> t -> t
  val equal : t -> t -> bool
end

module type S = sig
  include MIN
  include Atom.S with type t := t

  val times : t -> int -> t
  val is_zero : t -> bool
end

module Extend (M : MIN) = struct
  let is_zero x = M.equal x M.zero

  let times x n =
    let rec aux acc n = if n = 0 then acc else aux (M.add acc x) (n - 1) in
    aux M.zero n
end
