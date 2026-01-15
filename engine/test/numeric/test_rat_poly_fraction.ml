open Types
module R = Latex_reporter
module Poly = Rat_polynomial
module Frac = Rat_poly_fraction
module Rat = Rational

(* Helper to create a rational number *)
let r n = Rat.of_int n

(* Helper: create polynomial from coeff list *)
let poly coeffs = Poly.from_array (Array.of_list (List.map r coeffs))

(* Helper: create fraction from poly coeff lists *)
let pf nums dens =
  let p_num = poly nums in
  let p_den = poly dens in
  Frac.v (p_num, p_den)

let test () =
  let ts = Frac.to_string_latex in
  let poly_ts = Poly.to_string_latex in

  R.section "Rational Function Field (Fractions of Polynomials)";

  (* Constants *)
  let zero = Frac.zero in
  let one = Frac.one in

  R.subsection "Constants";
  R.eq (ts zero) "0" |> R.adds;
  R.eq (ts one) "1" |> R.adds;

  (* Basic Addition *)
  R.subsection "Addition";
  (* (x + 1)/x + 1/(x+1) = ((x+1)^2 + x) / x(x+1) = (x^2 + 3x + 1) / (x^2 + x) *)
  let f1 = pf [ 1; 1 ] [ 0; 1 ] in
  (* (x+1)/x *)
  let f2 = pf [ 1 ] [ 1; 1 ] in
  (* 1/(x+1) *)
  let sum = Frac.add f1 f2 in
  let expected_num = poly [ 1; 3; 1 ] in
  let expected_den = poly [ 0; 1; 1 ] in
  (* Normalized automatically? Fraction.Make does normalize using gcd *)
  (* Let's check the latex output *)
  R.eq (R.add (ts f1) (ts f2)) (ts sum) |> R.adds;

  (* Verify result structure if possible, or just visual check via latex *)
  let res_n, res_d = sum in
  R.eq (poly_ts res_n) (poly_ts expected_num) |> R.adds;
  R.eq (poly_ts res_d) (poly_ts expected_den) |> R.adds;

  (* Multiplication & Reduction *)
  R.subsection "Multiplication and Reduction";
  (* (x^2 - 1)/(x+1) * 1/(x-1) = (x-1)(x+1)/(x+1) * 1/(x-1) = 1 *)
  let fa = pf [ -1; 0; 1 ] [ 1; 1 ] in
  (* (x^2 - 1) / (x+1) *)
  let fb = pf [ 1 ] [ -1; 1 ] in
  (* 1 / (x-1) *)
  let prod = Frac.mul fa fb in

  R.eq (R.mul (ts fa) (ts fb)) (ts prod) |> R.adds;
  R.eq (ts prod) "1" |> R.adds;

  (* Division *)
  R.subsection "Division";
  (* ((x+1)/x) / ((x+1)/x^2) = (x+1)/x * x^2/(x+1) = x *)
  let f_div1 = pf [ 1; 1 ] [ 0; 1 ] in
  let f_div2 = pf [ 1; 1 ] [ 0; 0; 1 ] in
  let quot = Frac.div f_div1 f_div2 in
  let expected_quot = pf [ 0; 1 ] [ 1 ] in
  (* x/1 -> x *)

  R.eq (R.div (ts f_div1) (ts f_div2)) (ts quot) |> R.adds;
  R.eq (ts quot) (ts expected_quot) |> R.adds;
  (* Should match "x" *)
  R.eq (ts quot) "x" |> R.adds;

  (* Inverse *)
  R.subsection "Inverse";
  (* inv( (x+1)/x ) = x/(x+1) *)
  let f_inv = pf [ 1; 1 ] [ 0; 1 ] in
  let inv_res = Frac.inv f_inv in
  let expected_inv = pf [ 0; 1 ] [ 1; 1 ] in
  R.eq (R.inv (ts f_inv)) (ts inv_res) |> R.adds;
  R.eq (ts inv_res) (ts expected_inv) |> R.adds;

  (* Negative Handling *)
  R.subsection "Sign Normalization";
  (* ( -x ) / ( -1 ) = x *)
  (* Using Fraction.Make, signs are handled by Coeff.is_negative and abs *)
  (* If den is negative (leading coeff negative), both num and den are negated. *)
  let f_neg = pf [ 0; -1 ] [ -1 ] in
  (* -x / -1 *)
  (* Expect x *)
  R.eq (ts f_neg) "x" |> R.adds;

  (* (x) / (-x) = -1 *)
  let f_neg2 = pf [ 0; 1 ] [ 0; -1 ] in
  R.eq (ts f_neg2) "-1" |> R.adds;

  (* Differentiation *)
  R.subsection "Differentiation";
  (* ((x+1)/x)' = (x - (x+1))/x^2 = -1/x^2 *)
  let f_diff1 = pf [ 1; 1 ] [ 0; 1 ] in
  let f_diff1_res = Frac.derive f_diff1 in
  let f_diff1_expected = pf [ -1 ] [ 0; 0; 1 ] in

  R.eq (R.diff (ts f_diff1)) (ts f_diff1_res) |> R.adds;
  if not (Frac.equal f_diff1_res f_diff1_expected) then
    failwith "Differentiation failed for (x+1)/x";

  (* (1/(x^2+1))' = -2x/(x^2+1)^2 *)
  let f_diff2 = pf [ 1 ] [ 1; 0; 1 ] in
  let f_diff2_res = Frac.derive f_diff2 in
  let f_diff2_expected = pf [ 0; -2 ] [ 1; 0; 2; 0; 1 ] in
  (* (x^2+1)^2 = x^4 + 2x^2 + 1 -> [1; 0; 2; 0; 1] *)

  R.eq (R.diff (ts f_diff2)) (ts f_diff2_res) |> R.adds;
  if not (Frac.equal f_diff2_res f_diff2_expected) then
    failwith "Differentiation failed for 1/(x^2+1)";

  ()

let () =
  test ();
  print_string (R.get_full_latex_document ())
