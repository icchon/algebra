open Types
module R = Latex_reporter
module P = Rat_polynomial
module Rat = Rational

(* Helper to create a rational number *)
let r n = Rat.of_int n
let f n d = (n, d)

(* Helper to create a polynomial from int coefficients *)
(* [a; b; c] -> a + bx + cx^2 *)
let poly coeffs =
  let q_coeffs = Array.of_list (List.map r coeffs) in
  P.from_array q_coeffs

(* Helper to create a polynomial from fractional coefficients *)
let poly_f coeffs =
  let q_coeffs = Array.of_list coeffs in
  P.from_array q_coeffs

let test () =
  let ts = P.to_string_latex in

  R.section "Rational Polynomials";

  (* Define some polynomials *)
  let p1 = poly [ 1; 2; 3 ] in
  (* 1 + 2x + 3x^2 *)
  let p2 = poly [ 3; 2; 1 ] in
  (* 3 + 2x + x^2 *)
  (* Constants *)
  R.subsection "Constants";

  (* R.eq (ts zero) "0" |> R.adds; *)
  (* R.eq (ts one) "1" |> R.adds; *)

  (* Addition *)
  R.subsection "Addition";
  (* (1 + 2x + 3x^2) + (3 + 2x + x^2) = 4 + 4x + 4x^2 *)
  let sum = P.add p1 p2 in
  R.eq (R.add (ts p1) (ts p2)) (ts sum) |> R.adds;

  (* Addition with fractions *)
  (* (1/2 + x) + (1/3 - 1/2 x) = (1/2 + 1/3) + (1 - 1/2)x = 5/6 + 1/2 x *)
  let pf1 = poly_f [ f 1 2; f 1 1 ] in
  let pf2 = poly_f [ f 1 3; f (-1) 2 ] in
  let sum_f = P.add pf1 pf2 in
  R.eq (R.add (ts pf1) (ts pf2)) (ts sum_f) |> R.adds;

  (* Subtraction *)
  R.subsection "Subtraction";
  (* (1 + 2x + 3x^2) - (3 + 2x + x^2) = -2 + 0x + 2x^2 *)
  let diff = P.sub p1 p2 in
  R.eq (R.sub (ts p1) (ts p2)) (ts diff) |> R.adds;

  (* Multiplication *)
  R.subsection "Multiplication";
  (* (1 + x) * (1 - x) = 1 - x^2 *)
  let pa = poly [ 1; 1 ] in
  let pb = poly [ 1; -1 ] in
  let prod = P.mul pa pb in
  R.eq (R.mul (ts pa) (ts pb)) (ts prod) |> R.adds;

  (* Multiplication with fractions *)
  (* (1/2 x) * (1/3 x + 1) = 1/6 x^2 + 1/2 x *)
  let pf_a = poly_f [ f 0 1; f 1 2 ] in
  let pf_b = poly_f [ f 1 1; f 1 3 ] in
  let prod_f = P.mul pf_a pf_b in
  R.eq (R.mul (ts pf_a) (ts pf_b)) (ts prod_f) |> R.adds;

  (* Scaling *)
  R.subsection "Scalar Multiplication";
  (* 2 * (1 + x) = 2 + 2x *)
  let scalar = r 2 in
  let scaled = P.mul_scale pa scalar in
  R.eq (R.mul_scalar (Rat.to_string_latex scalar) (ts pa)) (ts scaled) |> R.adds;

  (* Evaluation *)
  R.subsection "Evaluation";
  (* Eval 1 + 2x + 3x^2 at x=2 -> 1 + 4 + 12 = 17 *)
  (* Using P.eval needs a module that matches Evaluatable signature.
     Rational matches Evaluatable where t = Rational.t *)
  let module RatEval = struct
    include Rational

    type s = t

    let mul_scale = mul
  end in
  let x_val = r 2 in
  let eval_res = P.eval (module RatEval) p1 x_val in
  (* Display: P(x) = ...; P(2) = 17 *)
  R.adds (R.eq_raw "P(x)" (ts p1));
  R.eq
    (Printf.sprintf "P(%s)" (Rat.to_string_latex x_val))
    (Rat.to_string_latex eval_res)
  |> R.adds;

  (* Division / Euclidean *)
  R.subsection "Euclidean Division (Ring Property)";
  (* (x^2 + 2x + 1) / (x + 1) = x + 1 rem 0 *)
  let dividend = poly [ 1; 2; 1 ] in
  (* (x+1)^2 *)
  let divisor = poly [ 1; 1 ] in
  (* x+1 *)
  let q = P.quo dividend divisor in
  let rem = P.rem dividend divisor in

  R.eq (R.quo (ts dividend) (ts divisor)) (ts q) |> R.adds;
  R.eq (R.rem (ts dividend) (ts divisor)) (ts rem) |> R.adds;

  (* Euclidean Division with fractions *)
  (* (1/2 x^2 + x) / (1/2 x) = x + 2 rem 0 ? No, lead coeffs matter. *)
  (* P.quo implements Euclidean division over a Field (Rational). *)
  (* (1/2 x^2 + x) = (1/2 x)(x + 2) *)
  let dividend_f = poly_f [ f 0 1; f 1 1; f 1 2 ] in
  let divisor_f = poly_f [ f 0 1; f 1 2 ] in
  let q_f = P.quo dividend_f divisor_f in
  let rem_f = P.rem dividend_f divisor_f in

  R.eq (R.quo (ts dividend_f) (ts divisor_f)) (ts q_f) |> R.adds;
  R.eq (R.rem (ts dividend_f) (ts divisor_f)) (ts rem_f) |> R.adds;

  (* GCD *)
  R.subsection "GCD";
  (* gcd(x^2 - 1, x^2 + 2x + 1) = x + 1 *)
  (* (x-1)(x+1), (x+1)^2 *)
  let p_gcd1 = poly [ -1; 0; 1 ] in
  let p_gcd2 = poly [ 1; 2; 1 ] in
  let gcd_val = P.gcd p_gcd1 p_gcd2 in
  (* GCD is monic, so x+1 is expected.
     The implementation of gcd in Polynomial.Make uses normalize_leading_coeff so it should be monic. *)
  R.eq (R.gcd (ts p_gcd1) (ts p_gcd2)) (ts gcd_val) |> R.adds;

  (* R.eq (ts gcd_val) (ts expected_gcd) |> R.adds; *)

  (* Composition *)
  R.subsection "Composition";
  (* P(Q(x)) *)
  (* 1. x \circ P = P *)
  let px = poly [ 0; 1 ] in
  let p_comp1 = P.compose px p1 in
  R.eq (R.compose (ts px) (ts p1)) (ts p_comp1) |> R.adds;

  (* 2. P \circ x = P *)
  let p_comp2 = P.compose p1 px in
  R.eq (R.compose (ts p1) (ts px)) (ts p_comp2) |> R.adds;

  (* 3. Complex: (1/2 x^2 + 1) \circ (2x + 1) *)
  (* P(x) = 1 + 1/2 x^2 *)
  (* Q(x) = 1 + 2x *)
  (* P(Q(x)) = 1 + 1/2 (1 + 2x)^2 = 1 + 1/2 (1 + 4x + 4x^2) = 1 + 1/2 + 2x + 2x^2 = 3/2 + 2x + 2x^2 *)
  let p_a = poly_f [ f 1 1; f 0 1; f 1 2 ] in
  let p_b = poly [ 1; 2 ] in
  let p_res = P.compose p_a p_b in

  R.eq (R.compose (ts p_a) (ts p_b)) (ts p_res) |> R.adds;

  (* Differentiation *)
  R.subsection "Differentiation";

  (* P(x) = x^3 + 2x^2 + 3x + 4 *)
  let p_diff = poly [ 4; 3; 2; 1 ] in

  let p_diff_res = P.derive p_diff in

  let p_diff_expected = poly [ 3; 4; 3 ] in

  R.eq (R.diff (ts p_diff)) (ts p_diff_res) |> R.adds;

  (* Verification against expected *)
  if not (P.equal p_diff_res p_diff_expected) then
    failwith "Differentiation failed for x^3 + 2x^2 + 3x + 4";

  (* Fractional coefficients *)

  (* (1/2 x^2 + 1/3 x)' = x + 1/3 *)
  let p_diff_f = poly_f [ f 0 1; f 1 3; f 1 2 ] in

  let p_diff_f_res = P.derive p_diff_f in

  let p_diff_f_expected = poly_f [ f 1 3; f 1 1 ] in

  R.eq (R.diff (ts p_diff_f)) (ts p_diff_f_res) |> R.adds;

  if not (P.equal p_diff_f_res p_diff_f_expected) then
    failwith "Differentiation failed for 1/2 x^2 + 1/3 x";

  (* Matrix Evaluation *)
  R.subsection "Matrix Evaluation";

  (* Let A = [[1, 1], [0, 1]] *)
  (* P(x) = x^2 - 2x + 1 = (x-1)^2 *)
  (* P(A) = (A - I)^2 = [[0, 1], [0, 0]]^2 = [[0, 0], [0, 0]] *)

  (* Define 2x2 Matrix Module *)
  let module D2 = struct
    let n = 2
  end in
  let module Mat2 = Square.Make (Rational) (D2) in
  (* Wrapper to satisfy Evaluatable *)
  let module Mat2Eval = struct
    include Mat2

    type s = Rational.t
  end in
  (* Create Matrix A *)
  let mat_a =
    Mat2.pure Rat.zero |> Mat2.map (fun _ -> Rat.zero)
    (* init with zero *)
  in
  mat_a.(0).(0) <- Rat.one;
  mat_a.(0).(1) <- Rat.one;
  mat_a.(1).(0) <- Rat.zero;
  mat_a.(1).(1) <- Rat.one;

  let poly_sq = poly [ 1; -2; 1 ] in
  (* x^2 - 2x + 1 *)
  let res_mat = P.eval (module Mat2Eval) poly_sq mat_a in

  (* Display P(x) *)
  R.adds (R.eq_raw "P(x)" (ts poly_sq));
  (* Display A *)
  R.adds (R.eq_raw "A" (Mat2.to_string_latex mat_a));

  R.eq "P(A)" (Mat2.to_string_latex res_mat) |> R.adds;

  (* Q(x) = x^2 *)
  (* Q(A) = A^2 = [[1, 2], [0, 1]] *)
  let poly_x2 = poly [ 0; 0; 1 ] in
  let res_mat2 = P.eval (module Mat2Eval) poly_x2 mat_a in

  R.adds (R.eq_raw "Q(x)" (ts poly_x2));

  R.eq "Q(A)" (Mat2.to_string_latex res_mat2) |> R.adds;

  ()

let () =
  test ();
  print_string (R.get_full_latex_document ())
