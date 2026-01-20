open Types
module Frac = Rootp_poly_fraction
open Frac
module Poly = RootP_polynomial
module RP = RootP
module R = Latex_reporter
open Printf

let r n = RP.of_int n
let poly coeffs = Poly.from_array (Array.map r coeffs)

let test () =
  let ts = to_string_latex in
  let poly_ts = Poly.to_string_latex in
  
  R.section "Complex Integration Examples in Q(root(p))(x)";

  let test_case f_name f =
    R.subsection f_name;
    let res = integrate f in
    
    let log_part_str = 
      if res.log_part = [] then "0"
      else 
        let terms = List.map (fun (c, p) -> 
          let c_str = RP.to_string_latex c in
          sprintf {|%s \log \left( %s \right)|} c_str (poly_ts p)
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

    let full_res = 
        if ts res.rational_part = "0" then log_part_str
        else if log_part_str = "0" then ts res.rational_part
        else 
            let r_str = ts res.rational_part in
            if String.length log_part_str > 0 && log_part_str.[0] = '-' then
                sprintf "%s - %s" r_str (String.sub log_part_str 1 (String.length log_part_str - 1))
            else
                sprintf "%s + %s" r_str log_part_str
    in
    R.eq (sprintf {| \int %s dx |} (ts f)) full_res |> R.adds
  in

  (* 1. Polynomial + Log: (x^4 + 1) / (x^2 + 1) = x^2 - 1 + 2/(x^2 + 1) *)
  let f1 = v (poly [| 1; 0; 0; 0; 1 |], poly [| 1; 0; 1 |]) in
  test_case "(x^4 + 1) / (x^2 + 1)" f1;
  
  (* 2. Hermite: x / (x^2 + 1)^2 *)
  let f2 = v (poly [| 0; 1 |], poly [| 1; 0; 2; 0; 1 |]) in
  test_case "x / (x^2 + 1)^2" f2;
  
  (* 3. Hermite + Log: 1 / (x^2 - 2)^2 *)
  let f3 = v (Poly.one, poly [| 4; 0; -4; 0; 1 |]) in
  test_case "1 / (x^2 - 2)^2" f3;

  (* 4. High power Hermite: 1 / x^5 *)
  let f4 = v (Poly.one, poly [| 0; 0; 0; 0; 0; 1 |]) in
  test_case "1 / x^5" f4;

  (* 5. Mixed: (x^3 + 1) / (x^2 + x + 1) *)
  let f5 = v (poly [| 1; 0; 0; 1 |], poly [| 1; 1; 1 |]) in
  test_case "(x^3 + 1) / (x^2 + x + 1)" f5;

  (* 6. Complex coefficients / Roots: 1 / (x^2 + 2x + 2) *)
  let f6 = v (Poly.one, poly [| 2; 2; 1 |]) in
  test_case "1 / (x^2 + 2x + 2)" f6;
  
  ()

let () =
  test ();
  print_string (R.get_full_latex_document ())