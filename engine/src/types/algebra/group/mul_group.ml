module type MIN = sig
  type t

  val mul : t -> t -> t
  val inv : t -> t
end

module type S = sig
  include MIN
  include Mul_monoid.S with type t := t

  val div : t -> t -> t
end

module Extend (M : MIN) = struct
  let div x y = M.mul x (M.inv y)
end
