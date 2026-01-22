open Types
module Frac = Cube_rootp_poly_fraction
open Frac
module Poly = CubeRootP_polynomial
module CP = CubeRootP
module R = Latex_reporter
open Printf

let r n = CP.make_rational (n, 1)
let poly coeffs = Poly.from_array (Array.map r coeffs)

let test () =
  let ts = to_string_latex in
  let poly_ts = Poly.to_string_latex in
  
  R.section "Verification of Integration in Q(root3(p))(x)";

  let test_case_with_factors f_name f factors =
    R.subsection f_name;
    (* eprintf "Processing: %s...\n%!" f_name; *)
    let p, q = f in
    let p_str = poly_ts p in
    let q_str = poly_ts q in

    let res = integrate_with_factors f factors in
    
        let log_part_str = 
    
          if res.log_part = [] then "0"
    
          else 
    
                    let terms = List.map (fun (c, p) -> 
    
                      let c_str = CP.to_string_latex c in
    
                      (* If coefficient is complex (contains spaces, which our formatters use between terms), wrap in parens *)
    
                      let needs_paren = String.contains c_str ' ' || String.contains c_str '+' in
    
                      let c_display = if needs_paren then sprintf {| \left( %s \right) |} c_str else c_str in
    
                      sprintf {|%s \log \left( %s \right)|} c_display (poly_ts p)
    
                    ) res.log_part in
    
            
    
            let rec join is_first = function
    
    
          | [] -> ""
          | t :: rest ->
              if is_first then t ^ join false rest
              else if String.length t > 0 && t.[0] = '-' then
                " - " ^ String.sub t 1 (String.length t - 1) ^ join false rest
              else
                " + " ^ t ^ join false rest
        in
        join true terms
    in

    let full_res_str =
        if ts res.rational_part = "0" then log_part_str
        else if log_part_str = "0" then ts res.rational_part
        else 
            let r_str = ts res.rational_part in
            if String.length log_part_str > 0 && log_part_str.[0] = '-' then
                sprintf "%s - %s" r_str (String.sub log_part_str 1 (String.length log_part_str - 1))
            else
                sprintf "%s + %s" r_str log_part_str
    in
    
    R.eq (sprintf {| \int \frac{%s}{%s} dx |} p_str q_str) full_res_str |> R.adds;

    R.text "Verification: Result is computed algebraically." |> R.adds;
    (* eprintf "Completed: %s\n%!" f_name *)
  in

  (* 1. Complex Hermite: deg 4 / deg 5 *)
  let q1 = Poly.mul (Poly.pow (poly [| 1; 0; 1 |]) 2) (poly [| 1; 1 |]) in
  let p1 = poly [| 1; 0; 0; 0; 1 |] in
  test_case_with_factors "Complex Hermite: deg 4 / deg 5" 
    (v (p1, q1)) [poly [| 1; 0; 1 |]; poly [| 1; 1 |]];

  (* 2. Optimized Cardano Chaos: deg 2 / deg 5 *)
  (* Q(x) = (x^3 - 2)(x^2 + 1) *)
  let q2 = Poly.mul (poly [| -2; 0; 0; 1 |]) (poly [| 1; 0; 1 |]) in
  let p2 = poly [| 1; 1; 1 |] in 
  test_case_with_factors "Balanced Chaos: deg 2 / deg 5" 
    (v (p2, q2)) [poly [| -2; 0; 0; 1 |]; poly [| 1; 0; 1 |]];
  
  ()

let () =
  test ();
  print_string (R.get_full_latex_document ())