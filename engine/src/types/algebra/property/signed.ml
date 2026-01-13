module type MIN = sig
  type t

  val is_positive : t -> bool
  val neg : t -> t
end

module type S = sig
  include MIN

  val abs : t -> t
end

module Extend (M : MIN) = struct
  let abs x = if M.is_positive x then x else M.neg x
end
