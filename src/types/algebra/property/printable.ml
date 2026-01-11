module type S = sig
  type t

  val to_string : t -> string
  val to_string_latex : t -> string
end
