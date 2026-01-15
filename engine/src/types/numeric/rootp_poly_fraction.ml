(* Use Q_rootp.DynamicNumber as the coefficient field *)
module RootP = struct
  include Q_rootp.DynamicNumber
  let to_string_latex_level _ x = to_string_latex x
  let is_negative = is_negative
  let compare = compare
end

module RootP_polynomial = struct
  include Polynomial.Make(RootP)
  let v x = x
  let from_array x = normalize x
end

module BASE = Fraction.Make(RootP_polynomial)

include BASE

(* Extended functionality required for Base module *)

let to_string_latex x = BASE.to_string_latex x

let derive x = BASE.derive x

let of_rootp r = BASE.v (RootP_polynomial.from_array [| r |], RootP_polynomial.one)

(* Helper to create simple polynomial in x with integer coefficients (converted to RootP) *)
let of_int_poly coeffs =
  let rootp_coeffs = Array.map RootP.of_int coeffs in
  let p = RootP_polynomial.from_array rootp_coeffs in
  BASE.v (p, RootP_polynomial.one)
