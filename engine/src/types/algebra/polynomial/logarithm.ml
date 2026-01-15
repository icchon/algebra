module Make
    (Coeff : sig
      include Field.S

      val to_string_latex_level : int -> t -> string
      val is_negative : t -> bool
      val is_one : t -> bool
      val derive : t -> t
      val compare : t -> t -> int
    end) (Param : sig

  val t_derive : Coeff.t
  val var_name_latex : string
end) =
struct
  (* 1. Inherit standard polynomial functionality *)
  module Poly = Polynomial.Make (Coeff)
  include Poly

  (* 2. Override derivation for Logarithm rules *)
  (* Let t = log(a). Then t' = a'/a (provided via Param.t_derive). *)
  (* (c_i * t^i)' = c_i' * t^i + c_i * i * t^(i-1) * t' *)
  (*              = c_i' * t^i + (c_i * i * t') * t^(i-1) *)

  let derive p =
    let len = Array.length p in
    if len = 0 then zero
    else
      (* Term 1: Coefficient derivatives sum( c_i' * t^i ) *)
      let term1 = map_coeff Coeff.derive p in
      (* Term 2: Power rule sum( (c_i * i * t') * t^(i-1) ) *)
      let term2 =
        if len <= 1 then zero
        else
          let new_arr = Array.make (len - 1) Coeff.zero in
          for i = 1 to len - 1 do
            (* index j = i-1 *)
            (* coeff at j is c_i * i * t' *)
            let ci = p.(i) in
            let deriv_part = Coeff.mul (Coeff.times ci i) Param.t_derive in
            new_arr.(i - 1) <- deriv_part
          done;
          normalize new_arr
      in
      add term1 term2

  (* 3. Custom Formatting *)
  include
    Formatter.GenLinear
      (Coeff)
      (struct
        type nonrec t = t

        let components p = Array.to_list p

        let bases _level len =
          List.init len (function
            | 0 -> ""
            | 1 -> Param.var_name_latex
            | i ->
                "\\left(" ^ Param.var_name_latex ^ "\\right)^{"
                ^ string_of_int i ^ "}")
      end)
end
