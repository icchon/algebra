module type S = sig
  type t

  val derive : t -> t
end
