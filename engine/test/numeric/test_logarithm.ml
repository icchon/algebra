open Types
module R = Latex_reporter

(* 1. Base Field: Rational Function Field Q(x) *)
module Base = Rat_poly_fraction

(* 2. Parameter for log(x): t' = 1/x *)
module LogXParam = struct
  let r n = Rational.of_int n

  let poly coeffs =
    Rat_polynomial.from_array (Array.of_list (List.map r coeffs))

  let x = poly [ 0; 1 ]
  let one = poly [ 1 ]
  let t_derive = Base.v (one, x) (* 1/x *)
  let var_name_latex = R.log "x"
end

(* 3. Extension: Polynomials in log(x) over Q(x) *)
module Log = Logarithm.Make (Base) (LogXParam)

(* Helpers *)
let r n = Rational.of_int n
let poly coeffs = Rat_polynomial.from_array (Array.of_list (List.map r coeffs))
let frac num den = Base.v (num, den)
let log_poly coeffs = Log.normalize (Array.of_list coeffs)

let test () =
  let ts = Log.to_string_latex in

  R.section "Logarithmic Extension";

  (* Constants in Q(x) *)
  let one_x = poly [ 0; 1 ] in
  let one = poly [ 1 ] in
  let c_x = frac one_x one in
  let c_1 = frac one one in
  let c_0 = Base.zero in

  (* Variable t = log x *)
  let t = log_poly [ c_0; c_1 ] in
  (* 0 + 1*log x *)

  (* Case 1: (log x)' = 1/x *)
  R.subsection "Derivative of log x";
  let dt = Log.derive t in
  R.eq (R.diff (ts t)) (ts dt) |> R.adds;

  let expected_dt = log_poly [ LogXParam.t_derive ] in
  if not (Log.equal dt expected_dt) then
    failwith "Differentiation failed for log x";

  (* Case 2: (x * log x)' = log x + 1 *)
  R.subsection "Derivative of product x log x";
  let xt = log_poly [ c_0; c_x ] in
  let dxt = Log.derive xt in
  R.eq (R.diff (ts xt)) (ts dxt) |> R.adds;

  let expected_dxt = log_poly [ c_1; c_1 ] in
  (* 1 + 1*log x *)
  if not (Log.equal dxt expected_dxt) then
    failwith "Differentiation failed for x log x";

  (* Case 3: ((log x)^2)' = 2(log x)/x *)
  R.subsection "Derivative of power (log x)^2";
  let t2 = log_poly [ c_0; c_0; c_1 ] in
  let dt2 = Log.derive t2 in
  R.eq (R.diff (ts t2)) (ts dt2) |> R.adds;

  let two_over_x =
    Base.mul (frac (poly [ 2 ]) (poly [ 1 ])) LogXParam.t_derive
  in
  let expected_dt2 = log_poly [ c_0; two_over_x ] in
  if not (Log.equal dt2 expected_dt2) then
    failwith "Differentiation failed for (log x)^2";

  ()

let () =
  test ();
  print_string (R.get_full_latex_document ())
