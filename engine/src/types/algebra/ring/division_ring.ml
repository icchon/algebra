module type MIN = sig
  type t

  val mul : t -> t -> t
  val pow : t -> int -> t
  val inv : t -> t
end

module type S = sig
  include MIN
  include Ring.S with type t := t
  include Mul_group.S with type t := t

  val div : t -> t -> t
  val pow : t -> int -> t
end

module Extend (M : MIN) = struct
  let div x y = M.mul x (M.inv y)
  let pow x n = if n >= 0 then M.pow x n else M.pow (M.inv x) (-n)
end
