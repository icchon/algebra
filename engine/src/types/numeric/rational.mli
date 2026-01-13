type t = int * int

val equal : t -> t -> bool
val comp : t -> t -> int
val to_string : t -> string
val to_string_latex : t -> string
val to_string_latex_level : int -> t -> string
val zero : t
val add : t -> t -> t
val one : t
val mul : t -> t -> t
val conj : t -> t

(* val quo: t -> t -> t
val rem: t -> t -> t *)
val max : t -> t -> t
val min : t -> t -> t
val is_positive : t -> bool
val is_negative : t -> bool
val dot : t -> t -> t
val norm_sq : t -> t
val is_one : t -> bool
val is_zero : t -> bool
val times : t -> int -> t
val pow : t -> int -> t
val pow_non_neg : t -> int -> t
val v : t -> t
val neg : t -> t
val sub : t -> t -> t

(* val gcd: t -> t -> t
val gcd_ext: t -> t -> t*t*t *)
val abs : t -> t
val inv : t -> t
val div : t -> t -> t
val of_int : Integer.t -> t
val cubic_root : t -> t option
val square_root : t -> t option

val of_yojson: Yojson.Safe.t -> t
