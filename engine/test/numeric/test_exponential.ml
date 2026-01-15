open Types
module R = Latex_reporter

(* 1. Base Field: Q(x) *)
module Base = Rat_poly_fraction

(* 2. Extension 1: t = e^x. f=x, f'=1 *)
module ExpXParam = struct
  let f_derive = Base.one
  let var_name_latex = R.exp "x"
end

module ExpX = Exponential.Make (Base) (ExpXParam)
module ExpXField = Fraction.Make (ExpX)

(* 3. Extension 2: u = e^{e^x}. f=e^x, f'=e^x *)
module ExpExpParam = struct
  (* f' = e^x in ExpXField *)
  let f_derive_ring = ExpX.normalize [| Base.zero; Base.one |]
  let f_derive = ExpXField.v (f_derive_ring, ExpX.one)
  let var_name_latex = R.exp (ExpX.to_string_latex f_derive_ring)
end

module ExpExp = Exponential.Make (ExpXField) (ExpExpParam)

(* 4. Extension 3: v = e^{2x}. f=2x, f'=2 *)
module Exp2XParam = struct
  let r n = Rational.of_int n
  let two = Rat_polynomial.from_array [| r 2 |]
  let f_derive = Base.v (two, Rat_polynomial.one) (* 2 *)
  let var_name_latex = R.exp "2x"
end

module Exp2X = Exponential.Make (Base) (Exp2XParam)

(* Helpers *)
let poly_x coeffs =
  Rat_polynomial.from_array (Array.of_list (List.map Rational.of_int coeffs))

let test () =
  let ts_x = ExpX.to_string_latex in
  let ts_xf = ExpXField.to_string_latex in
  let ts_ee = ExpExp.to_string_latex in
  let ts_2x = Exp2X.to_string_latex in

  R.section "Complex Exponential Arithmetic and Differentiation";

  (* Case 1: Arithmetic in Q(x)(e^x) *)
  R.subsection "Fractional Arithmetic";
  (* A = e^x / (e^x + 1) *)
  let t = ExpX.normalize [| Base.zero; Base.one |] in
  let t_plus_1 = ExpX.normalize [| Base.one; Base.one |] in
  let fa = ExpXField.v (t, t_plus_1) in

  (* B = 1 / e^x *)
  let fb = ExpXField.v (ExpX.one, t) in

  (* A + B = (t^2 + t + 1) / (t(t+1)) *)
  let f_sum = ExpXField.add fa fb in
  R.eq (R.add (ts_xf fa) (ts_xf fb)) (ts_xf f_sum) |> R.adds;

  (* Case 2: Complex Differentiation in Q(x)(e^x) *)
  R.subsection "Quotient Rule Differentiation";
  (* f = e^x / (e^x + x) *)
  let x_in_base =
    Base.v
      ( Rat_polynomial.from_array [| Rational.zero; Rational.one |],
        Rat_polynomial.one )
  in
  let t_plus_x = ExpX.normalize [| x_in_base; Base.one |] in
  let f = ExpXField.v (t, t_plus_x) in

  let df = ExpXField.derive f in
  R.eq (R.diff (ts_xf f)) (ts_xf df) |> R.adds;

  (* Verification of result structure *)
  (* df should be ((x-1)e^x) / (e^x + x)^2 *)
  let x_minus_1 =
    Base.v
      ( Rat_polynomial.from_array [| Rational.of_int (-1); Rational.one |],
        Rat_polynomial.one )
  in
  let num_expected = ExpX.normalize [| Base.zero; x_minus_1 |] in
  let den_expected = ExpX.mul t_plus_x t_plus_x in
  let f_expected = ExpXField.v (num_expected, den_expected) in

  if not (ExpXField.equal df f_expected) then
    failwith "Differentiation failed for e^x/(e^x+x)";

  (* Case 3: Nested Extension Q(x)(e^x)(e^{e^x}) *)
  R.subsection "Nested Extension Differentiation";
  (* g = x * e^{e^x} *)
  (* In ExpExp, this is coefficient c1 * u^1 where c1 = x (from ExpXField) *)
  let x_in_exp_field = ExpXField.v (ExpX.pure x_in_base, ExpX.one) in
  let g = ExpExp.normalize [| ExpXField.zero; x_in_exp_field |] in

  let dg = ExpExp.derive g in
  R.eq (R.diff (ts_ee g)) (ts_ee dg) |> R.adds;

  (* g' = (x)' e^{e^x} + x (e^{e^x})' = 1 * e^{e^x} + x * e^x * e^{e^x} = (1 + x e^x) e^{e^x} *)
  let e_x_in_field = ExpExpParam.f_derive in
  let c_term =
    ExpXField.add ExpXField.one (ExpXField.mul x_in_exp_field e_x_in_field)
  in
  let g_expected = ExpExp.normalize [| ExpXField.zero; c_term |] in

  if not (ExpExp.equal dg g_expected) then
    failwith "Nested differentiation failed";

  (* Case 4: Demonstrating Normalization Issues *)
  R.section "Normalization Challenges";
  R.subsection "Comparing (e^x)^2 and e^{2x}";

  let ex_sq = ExpX.normalize [| Base.zero; Base.zero; Base.one |] in
  let e2x = Exp2X.normalize [| Base.zero; Base.one |] in

  R.adds (R.text "The square of the first variable:");
  R.adds (R.eq_raw "f" (ts_x ex_sq));
  R.adds (R.text "The second variable introduced independently:");
  R.adds (R.eq_raw "g" (ts_2x e2x));

  R.adds
    (R.text
       "Their derivatives are mathematically identical but structurally \
        different:");
  R.eq (R.diff (ts_x ex_sq)) (ts_x (ExpX.derive ex_sq)) |> R.adds;
  R.eq (R.diff (ts_2x e2x)) (ts_2x (Exp2X.derive e2x)) |> R.adds;

  (* Case 5: Demonstration of dependency failure *)
  R.section "Algebraic Dependency Failure";
  R.subsection "e^{2x} vs (e^x)^2 in nested ring";

  let module ExpMixedParam = struct
    let f_derive =
      ExpXField.v (ExpX.pure (Base.v (poly_x [ 2 ], poly_x [ 1 ])), ExpX.one)

    let var_name_latex = R.exp "2x"
  end in
  let module ExpMixed = Exponential.Make (ExpXField) (ExpMixedParam) in
  let ts_m = ExpMixed.to_string_latex in

  (* f = 1 * e^{2x} - (e^x)^2 *)
  let ex_sq_field = ExpXField.v (ex_sq, ExpX.one) in
  let f = ExpMixed.normalize [| ExpXField.neg ex_sq_field; ExpXField.one |] in

  R.adds (R.text "In a nested ring where they are treated as independent:");
  R.adds (R.eq_raw "h" (ts_m f));

  if ExpMixed.is_zero f then
    failwith "Unexpectedly identified e^{2x} and (e^x)^2"
  else R.adds (R.text "The system does NOT identify this as zero.");

  (* Derivative also remains non-zero *)
  let df = ExpMixed.derive f in
  R.eq (R.diff (ts_m f)) (ts_m df) |> R.adds;

  if ExpMixed.is_zero df then failwith "Derivative unexpectedly became zero"
  else R.adds (R.text "The derivative is also not zero in this representation.");

  ()

let () =
  test ();
  print_string (R.get_full_latex_document ())
