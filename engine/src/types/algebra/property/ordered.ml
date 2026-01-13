module type MIN = sig
  type t

  val zero : t
  val comp : t -> t -> int
end

module type S = sig
  include MIN

  val max : t -> t -> t
  val min : t -> t -> t
  val is_positive : t -> bool
  val is_negative : t -> bool
end

module Extend (M : MIN) = struct
  let max x y = if M.comp x y >= 0 then x else y
  let min x y = if M.comp x y <= 0 then x else y
  let is_positive x = M.comp x M.zero >= 0
  let is_negative x = is_positive x |> not
end
