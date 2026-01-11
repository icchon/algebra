module type S = sig
  type t

  val equal : t -> t -> bool

  include Involutive.S with type t := t
  include Printable.S with type t := t
end
