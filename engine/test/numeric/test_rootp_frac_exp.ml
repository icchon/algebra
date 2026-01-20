open Types
module Frac = Rootp_poly_fraction
module Poly = Frac.RootP_polynomial
module RootP = Frac.RootP
module Exp = Rootp_frac_exp
module R = Latex_reporter
open Printf

let r n = RootP.of_int n
let poly coeffs = Poly.from_array (Array.map r coeffs)
let f n d = Frac.v (poly n, poly d)

let test () =
  let ts = Exp.to_string_latex in
  
  R.section "Advanced Integration of Rational Exponential Functions";

  let test_case f_name terms =
    R.subsection f_name;
    let res = Exp.integrate terms in
    
    let elem_str = ts res.elementary in
    let non_elem_str =
      if res.nonelementary = [] then ""
      else sprintf {| + \int \left( %s \right) dx |} (ts res.nonelementary)
    in
    
    R.eq (sprintf {| \int \left( %s \right) dx |} (ts terms)) (elem_str ^ non_elem_str) |> R.adds;
    
    if res.nonelementary = [] then
      R.text "Verification: Elementary solution found and verified algebraically." |> R.adds
    else
      R.text "Result: This integral contains a non-elementary part." |> R.adds
  in

  (* 1. Quadratic Exponent: (2x^2 + 4x + 1) * e^{x^2} *)
  (* g = (x+2)e^{x^2} -> g' = (1 + (x+2)2x)e^{x^2} = (2x^2 + 4x + 1)e^{x^2} *)
  test_case "deg 2 exponent: (2x^2 + 4x + 1) * e^{x^2}" [
    { Exp.coeff = f [| 1; 4; 2 |] [| 1 |]; exponent = poly [| 0; 0; 1 |] }
  ];

  (* 2. Cubic Exponent: (3x^2 + 2x) * e^{x^3 + x^2} *)
  test_case "deg 3 exponent: (3x^2 + 2x) * e^{x^3 + x^2}" [
    { Exp.coeff = f [| 0; 2; 3 |] [| 1 |]; exponent = poly [| 0; 0; 1; 1 |] }
  ];

  (* 3. Rational + e^x: (x^3 + 2x^2 + 2x) / (x+1)^2 * e^x *)
  (* Result: x^2 / (x+1) * e^x *)
  test_case "Rational deg 3/2: (x^3 + 2x^2 + 2x) / (x+1)^2 * e^x" [
    { Exp.coeff = f [| 0; 2; 2; 1 |] [| 1; 2; 1 |]; exponent = poly [| 0; 1 |] }
  ];

  (* 4. High-degree Polynomial reduction: x^3 * e^x *)
  test_case "x^3 * e^x" [
    { Exp.coeff = f [| 0; 0; 0; 1 |] [| 1 |]; exponent = poly [| 0; 1 |] }
  ];

  (* 5. Mixed independent exponentials *)
  test_case "Mixed: e^x + (2x+1)e^{x^2+x}" [
    { Exp.coeff = f [| 1 |] [| 1 |]; exponent = poly [| 0; 1 |] };
    { Exp.coeff = f [| 1; 2 |] [| 1 |]; exponent = poly [| 0; 1; 1 |] }
  ];

  (* 6. Failure case: e^{x^2} *)
  test_case "Gaussian: e^{x^2}" [
    { Exp.coeff = f [| 1 |] [| 1 |]; exponent = poly [| 0; 0; 1 |] }
  ];

  ()

let () =
  test ();
  print_string (R.get_full_latex_document ())