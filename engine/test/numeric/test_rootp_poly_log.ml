open Types
module Frac = Rootp_poly_fraction
module Poly = Frac.RootP_polynomial
module RootP = Frac.RootP
module Log = Rootp_poly_log
module R = Latex_reporter
open Printf

let r n = RootP.of_int n
let poly coeffs = Poly.from_array (Array.map r coeffs)

let test () =
  let ts = Log.to_string_latex in
  
  R.section "Integration of Logarithmic Polynomials (Integration by Parts)";

  let test_case f_name terms =
    R.subsection f_name;
    let res = Log.integrate_expr terms in
    
    let integrand_str =
      String.concat " + " (List.map (fun (p, q) ->
        let p_str = Poly.to_string_latex p in
        let q_str = Poly.to_string_latex q in
        sprintf {|%s \log \left( %s \right)|} p_str q_str
      ) terms)
    in

    R.eq (sprintf {| \int \left( %s \right) dx |} integrand_str) (ts res) |> R.adds;
    R.text "Verification: Integration by parts was applied using the rational integrator." |> R.adds
  in

  (* 1. Basic: log(x) *)
  test_case "log(x)" [ (poly [|1|], poly [|0; 1|]) ];

  (* 2. x * log(x) *)
  test_case "x * log(x)" [ (poly [|0; 1|], poly [|0; 1|]) ];

  (* 3. log(x^2 + 1) *)
  test_case "log(x^2 + 1)" [ (poly [|1|], poly [|1; 0; 1|]) ];

  (* 4. Mixed: x * log(x^2 - 2) *)
  test_case "x * log(x^2 - 2)" [ (poly [|0; 1|], poly [|-2; 0; 1|]) ];

  ()

let () =
  test ();
  print_string (R.get_full_latex_document ())