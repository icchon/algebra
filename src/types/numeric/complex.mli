type e = Rational.t
type t = e Container.Tup2.t
type leaf = Rational.t

val equal : t -> t -> bool
val to_string : t -> string
val to_string_latex : t -> string
val to_string_latex_level : int -> t -> string
val zero : t
val add : t -> t -> t
val one : t
val mul : t -> t -> t
val conj : t -> t
val dot : t -> t -> e
val norm_sq : t -> e
val is_one : t -> bool
val is_zero : t -> bool
val times : t -> int -> t
val pow : t -> int -> t
val pow_non_neg : t -> int -> t
val v : t -> t
val neg : t -> t
val sub : t -> t -> t
val inv : t -> t
val div : t -> t -> t
val flatten : t -> leaf list

type s = Rational.t

val mul_scale : t -> s -> t
val div_scale : t -> s -> t
