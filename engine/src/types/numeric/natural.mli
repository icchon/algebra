type t = int

val equal : t -> t -> bool
val comp : t -> t -> int
val to_string : t -> string
val to_string_latex : t -> string
val to_string_latex_level : int -> t -> string
val zero : t
val add : t -> t -> t
val one : t
val mul : t -> t -> t
val times : t -> int -> t
val conj : t -> t
val quo : t -> t -> t
val rem : t -> t -> t
val max : t -> t -> t
val min : t -> t -> t
val is_positive : t -> bool
val is_negative : t -> bool
val dot : t -> t -> t
val norm_sq : t -> t
val is_one : t -> bool
val is_zero : t -> bool
val pow : t -> int -> t
val pow_non_neg : t -> int -> t
val v : t -> t
