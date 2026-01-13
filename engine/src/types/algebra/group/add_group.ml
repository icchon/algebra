module type MIN = sig
  type t

  val add : t -> t -> t
  val neg : t -> t
end

module type S = sig
  include MIN
  include Add_monoid.S with type t := t

  val sub : t -> t -> t
end

module Extend (M : MIN) = struct
  let sub x y = M.add x (M.neg y)
end
