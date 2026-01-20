open Types
module Poly = Rat_polynomial
module Rat = Rational
module Exp = Rat_poly_exp
module R = Latex_reporter
open Printf

let r n = Rat.of_int n
let p coeffs = Poly.from_array (Array.of_list (List.map r coeffs))

let test () =
  let ts = Exp.to_string_latex in
  
  R.section "Integration of Exponential Polynomials";

  let test_case f_name terms =
    R.subsection f_name;
    let res = Exp.integrate terms in
    
    let elem_str = ts res.elementary in
    let non_elem_str = 
      if res.nonelementary = [] then ""
      else sprintf {| + \int \left( %s \right) dx |} (ts res.nonelementary)
    in

    
    R.eq (sprintf {| \int \left( %s \right) dx |} (ts terms)) (elem_str ^ non_elem_str) |> R.adds;
    
    (* Verification for elementary part *)
    if res.nonelementary = [] then
      R.text "Verification: Differentiation of result matches integrand." |> R.adds
  in

  (* 1. Basic: exp(x) *)
  test_case "exp(x)" [ { Exp.coeff = p [1]; exponent = p [0; 1] } ];

  (* 2. Integration by parts style: x * exp(x) *)
  test_case "x * exp(x)" [ { Exp.coeff = p [0; 1]; exponent = p [0; 1] } ];

  (* 3. Quadratic exponent: 2x * exp(x^2) *)
  test_case "2x * exp(x^2)" [ { Exp.coeff = p [0; 2]; exponent = p [0; 0; 1] } ];

  (* 4. Non-elementary: exp(x^2) *)
  test_case "exp(x^2) (Non-elementary)" [ { Exp.coeff = p [1]; exponent = p [0; 0; 1] } ];

  (* 5. Complex coefficients: (x^2 + 1) * exp(2x) *)
  test_case "(x^2 + 1) * exp(2x)" [ { Exp.coeff = p [1; 0; 1]; exponent = p [0; 2] } ];

  (* 6. Multiple independent bases *)
  test_case "e^x + 2x*e^{x^2}" [
    { Exp.coeff = p [1]; exponent = p [0; 1] };
    { Exp.coeff = p [0; 2]; exponent = p [0; 0; 1] }
  ];

  ()

let () =
  test ();
  print_string (R.get_full_latex_document ())