open Types
module DE = Linear_de_solver
module RootP = Rootp_poly_fraction.RootP
module Poly = Rootp_poly_fraction.RootP_polynomial
module C = Q_cube_rootp.CubicNumber
module R = Latex_reporter

let test () =
  R.section "Linear Differential Equation Solver: Comprehensive Tests (Orders 1-3)";

  let test_case name coeffs_val f_terms =
    R.subsection name;
    let coeffs_rootp = Array.map RootP.of_int coeffs_val in
    let eq = { DE.coeffs = coeffs_rootp; forcing = f_terms } in
    let sol = DE.solve eq in
    
    let coeffs_str = Array.map string_of_int coeffs_val in
    let f_str = DE.to_string_latex f_terms in
    
    R.adds (R.linear_de coeffs_str f_str);
    R.eq "y(x)" (DE.to_string_latex_full sol) |> R.adds
  in

  (* 1st Order: y' + 2y = e^x *)
  test_case "1st Order" [| 2; 1 |] [
    { DE.poly = Poly.one; alpha = C.one }
  ];

  (* 2nd Order: y'' + y = sin(x) -> forcing e^{ix} *)
  (* Since we use RootP, let's use i = sqrt(-1) *)
  let i_root = C.of_q_rootp (RootP.make_root (-1)) in
  test_case "2nd Order Harmonic" [| 1; 0; 1 |] [
    { DE.poly = Poly.one; alpha = i_root }
  ];

  (* 3rd Order Complex: y''' - 3y'' + 3y' - y = (x^2 + 1) e^x *)
  (* Resonance mult = 3, so yp involves x^3, x^4, x^5 *)
  test_case "3rd Order Triple Root Resonance" [| -1; 3; -3; 1 |] [
    { DE.poly = Poly.from_array [| RootP.one; RootP.zero; RootP.one |]; alpha = C.one }
  ];

  (* Mixed Forcing: y' + y = x + e^x *)
  test_case "Mixed Forcing" [| 1; 1 |] [
    { DE.poly = Poly.from_array [| RootP.zero; RootP.one |]; alpha = C.zero };
    { DE.poly = Poly.one; alpha = C.one }
  ];

  ()

let () =
  test ();
  print_string (R.get_full_latex_document ())