module Make (Coeff : sig
  include Field.S

  val to_string_latex_level : int -> t -> string
  val is_negative : t -> bool
  val is_one : t -> bool
  val derive : t -> t
  val compare : t -> t -> int
end) (Param : sig
  val f_derive : Coeff.t (* f' where t = exp(f) *)
  val var_name_latex : string
end) =
struct
  (* 1. Inherit standard polynomial functionality *)
  module Poly = Polynomial.Make (Coeff)
  include Poly

  (* 2. Override derivation for Exponential rules *)
  (* Let t = exp(f). Then t' = f' * t. *)
  (* (c_i * t^i)' = c_i' * t^i + c_i * i * t^(i-1) * (f' * t) *)
  (*              = c_i' * t^i + (c_i * i * f') * t^i *)
  (*              = (c_i' + i * f' * c_i) * t^i *)

  let derive p =
    let len = Array.length p in
    if len = 0 then zero
    else
      let new_coeffs = Array.make len Coeff.zero in
      for i = 0 to len - 1 do
        let coeff = p.(i) in
        (* Term 1: Coefficient derivative c_i' *)
        let da = Coeff.derive coeff in
        (* Term 2: Power derivative contribution c_i * i * f' *)
        let dp = Coeff.mul (Coeff.times coeff i) Param.f_derive in

        new_coeffs.(i) <- Coeff.add da dp
      done;
      (* Re-normalize because cancellations might occur *)
      Poly.normalize new_coeffs

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
