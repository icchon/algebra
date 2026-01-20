open Yojson.Safe.Util

(* LinearE Implementation *)
module LinearE = struct
  type coeff = int * int

  (* [n; d] -> (n, d) *)
  let decode_coeff json =
    let l = to_list json in
    (to_int (List.nth l 0), to_int (List.nth l 1))

  (* { left: [...], right: [...] } -> (left_coeffs, right_coeffs) *)
  let decode_input json =
    let left = json |> member "left" |> to_list |> List.map decode_coeff in
    let right = json |> member "right" |> to_list |> List.map decode_coeff in
    (left, right)

  let solve (left : coeff list) (right : coeff list) =
    let sub (n1, d1) (n2, d2) = Types.Rational.sub (n1, d1) (n2, d2) in
    
    (* Handle variable length lists *)
    let len_l = List.length left in
    let len_r = List.length right in
    let max_len = max len_l len_r in
    
    let pad l len = l @ List.init (len - List.length l) (fun _ -> (0, 1)) in
    
    let l_pad = pad left max_len in
    let r_pad = pad right max_len in
    
    (* (left - right) = 0 *)
    let combined = List.map2 sub l_pad r_pad in
    
    let degree = List.length combined - 1 in
    
    if degree > 3 then
      [] (* Not implemented for degree > 3 *)
    else
      let padded_coeffs = pad combined 4 in
      let coeffs = Array.of_list padded_coeffs in
      
      let d = coeffs.(0) in (* Constant term *)
      let c = coeffs.(1) in (* x^1 *)
      let b = coeffs.(2) in (* x^2 *)
      let a = coeffs.(3) in (* x^3 *)

      Types.Cardano_solver.solve_cubic_raw a b c d
end

(* Helper to print full LaTeX document *)
let print_full_latex content =
  Printf.printf "\\documentclass[12pt]{article}\n";
  Printf.printf "\\usepackage[utf8]{inputenc}\n";
  Printf.printf "\\usepackage{amsmath}\n";
  Printf.printf "\\usepackage{amssymb}\n";
  Printf.printf "\\usepackage{geometry}\n";
  Printf.printf "\\geometry{a4paper, margin=1in}\n";
  Printf.printf "\\begin{document}\n";
  Printf.printf "%s\n" content;
  Printf.printf "\\end{document}\n";
  flush Stdlib.stdout

let wrap_align content =
  Printf.sprintf "\\begin{align*}\n%s\n\\end{align*}" content

let wrap_equation content =
  Printf.sprintf "\\begin{equation*}\n%s\n\\end{equation*}" content

(* LinearE Runner *)
module LinearERunner = struct
  let coeffs_to_latex coeffs =
    let terms = List.mapi (fun i (n, d) ->
      if n = 0 then "" else
      let c_val = Types.Rootp_poly_fraction.RootP.make_rational (n, d) in
      let c_str = Types.Q_cube_rootp.to_string_latex (Types.Q_cube_rootp.of_q_rootp c_val) in
      let var_part = if i = 0 then "" else if i = 1 then "x" else Printf.sprintf "x^{%d}" i in
      if c_str = "1" && i > 0 then var_part
      else if c_str = "-1" && i > 0 then "-" ^ var_part
      else if String.contains c_str '/' then Printf.sprintf "\\left( %s \\right)%s" c_str var_part
      else c_str ^ var_part
    ) coeffs in
    let valid = List.filter ((<>) "") terms in
    if valid = [] then "0"
    else
      let rec join is_first list =
        match list with
        | [] -> ""
        | t :: ts ->
            let prefix = 
              if is_first then (if t.[0] = '-' then "-" else "")
              else (if t.[0] = '-' then " - " else " + ")
            in
            let body = if t.[0] = '-' || t.[0] = '+' then String.sub t 1 (String.length t - 1) else t in
            prefix ^ body ^ join false ts
      in join true valid

  let run () =
    try
      while true do
        let line = read_line () |> String.trim in
        if line <> "" then
          try
            let json_str =
              if line.[0] = '{' then
                line
              else
                let ic = open_in line in
                let content = really_input_string ic (in_channel_length ic) in
                close_in ic;
                content
            in
            let json = Yojson.Safe.from_string json_str in
            let (left, right) = LinearE.decode_input json in
            let solutions = LinearE.solve left right in
            
            let left_latex = coeffs_to_latex left in
            let right_latex = coeffs_to_latex right in
            
            (* Calculate normalized form *)
            let sub (n1, d1) (n2, d2) = Types.Rational.sub (n1, d1) (n2, d2) in
            let len_l = List.length left in
            let len_r = List.length right in
            let max_len = max len_l len_r in
            let pad l len = l @ List.init (len - List.length l) (fun _ -> (0, 1)) in
            let l_pad = pad left max_len in
            let r_pad = pad right max_len in
            let combined = List.map2 sub l_pad r_pad in
            let norm_latex = coeffs_to_latex combined in

            let solutions_line =
              if solutions = [] then
                let degree = List.length combined - 1 in
                if degree > 3 then
                  "\\implies \\text{not implemented for degree > 3}"
                else
                  "\\implies x \\in \\emptyset"
              else
                let sols_formatted = List.mapi (fun i sol ->
                  Printf.sprintf "x_{ %d } = %s" (i + 1) (Types.Q_cube_rootp.to_string_latex sol)
                ) solutions in
                Printf.sprintf "\\implies \\left\\{ %s \\right\\}" (String.concat ", " sols_formatted)
            in

            let lines = 
              if right_latex = "0" then
                [ Printf.sprintf "%s &= 0" left_latex; solutions_line ]
              else
                [ Printf.sprintf "%s &= %s" left_latex right_latex;
                  Printf.sprintf "\\iff %s &= 0" norm_latex;
                  solutions_line ]
            in
            
            print_full_latex (wrap_align (String.concat "\\\\\n" lines))
          with e ->
             print_full_latex (Printf.sprintf "Error: %s" (Printexc.to_string e))
      done
    with End_of_file -> ()
end

(* LinearDE Implementation *)
module LinearDE = struct
  open Types.Linear_de_solver
  module RootP = Types.Rootp_poly_fraction.RootP
  module Poly = Types.Rootp_poly_fraction.RootP_polynomial
  module C = Types.Q_cube_rootp.CubicNumber

  let decode_rational json =
    let l = to_list json in
    (to_int (List.nth l 0), to_int (List.nth l 1))

  let decode_rootp json =
    let r = decode_rational json in
    RootP.make_rational r

  let decode_poly json =
    let coeffs = json |> to_list |> List.map decode_rootp |> Array.of_list in
    Poly.from_array coeffs

  let decode_forcing_term json =
    let poly_json = json |> member "poly" in
    let alpha_json = json |> member "alpha" in
    {
      poly = decode_poly poly_json;
      alpha = C.of_q_rootp (decode_rootp alpha_json);
    }

  let decode_input json =
    let coeffs = json |> member "coeffs" |> to_list |> List.map decode_rootp |> Array.of_list in
    let forcing = json |> member "forcing" |> to_list |> List.map decode_forcing_term in
    { coeffs; forcing }

  let needs_parens s =
    let s = String.trim s in
    if s = "" then false else
    let has_plus = String.contains s '+' in
    let has_minus = 
      try 
        let i = String.index s '-' in 
        i > 0 (* Minus not at the start *)
      with Not_found -> false 
    in
    has_plus || has_minus

  let lhs_to_latex coeffs =
    let terms = Array.mapi (fun i c ->
      let c_str = Types.Rootp_poly_fraction.RootP.to_string_latex c in
      if c_str = "0" then "" else
      let deriv = match i with 0 -> "y" | 1 -> "y'" | 2 -> "y''" | 3 -> "y'''" | k -> Printf.sprintf "y^{(%d)}" k in
      if c_str = "1" then deriv
      else if c_str = "-1" then "-" ^ deriv
      else if needs_parens c_str then Printf.sprintf "\\left( %s \\right)%s" c_str deriv
      else c_str ^ deriv
    ) coeffs |> Array.to_list in
    let valid = List.filter ((<>) "") terms in
    if valid = [] then "0"
    else
      let rec join is_first list =
        match list with
        | [] -> ""
        | t :: ts ->
            let prefix = if is_first then (if t.[0] = '-' then "-" else "") else (if t.[0] = '-' then " - " else " + ") in
            let body = if t.[0] = '-' || t.[0] = '+' then String.sub t 1 (String.length t - 1) else t in
            prefix ^ body ^ join false ts
      in join true (List.rev valid)

  let rhs_to_latex forcing =
    let terms = List.map (fun {poly; alpha} ->
      let p_str = Poly.to_string_latex poly in
      let a_str = C.to_string_latex alpha in
      let exp_part = if a_str = "0" then "" else if a_str = "1" then "e^x" else Printf.sprintf "e^{%sx}" a_str in
      if p_str = "0" then "0"
      else if p_str = "1" then (if exp_part = "" then "1" else exp_part)
      else if exp_part = "" then p_str
      else if needs_parens p_str then Printf.sprintf "\\left( %s \\right)%s" p_str exp_part
      else p_str ^ exp_part
    ) forcing in
    if terms = [] then "0" else String.concat " + " terms

  let run () =
    try
      while true do
        let line = read_line () |> String.trim in
        if line <> "" then
          try
            let json_str =
              if line.[0] = '{' then
                line
              else
                let ic = open_in line in
                let content = really_input_string ic (in_channel_length ic) in
                close_in ic;
                content
            in
            let json = Yojson.Safe.from_string json_str in
            let eq = decode_input json in
            let sol = solve eq in
            let rhs_tex = to_string_latex_full sol in
            
            let lhs = lhs_to_latex eq.coeffs in
            let rhs = rhs_to_latex eq.forcing in
            
            let lines =
              [ Printf.sprintf "%s &= %s" lhs rhs;
                Printf.sprintf "y(x) &= %s" rhs_tex ]
            in
            print_full_latex (wrap_align (String.concat "\\\\\n" lines))
          with e ->
            print_full_latex (Printf.sprintf "Error: %s" (Printexc.to_string e))
      done
    with End_of_file -> ()
end

(* Integral Implementation *)
module Integral = struct
  open Types.Rootp_frac_exp
  module Frac = Types.Rootp_poly_fraction
  module Poly = Types.Rootp_poly_fraction.RootP_polynomial
  module RootP = Types.Rootp_poly_fraction.RootP
  module LogInt = Types.Rootp_poly_log

  type method_type = 
    | Exp of Types.Rootp_frac_exp.term list
    | Log of (Poly.t * Poly.t) list

  (* Reuse decoding logic *)
  let decode_rational json =
    let l = to_list json in
    (to_int (List.nth l 0), to_int (List.nth l 1))

  let decode_rootp json =
    let r = decode_rational json in
    RootP.make_rational r

  let decode_poly json =
    let coeffs = json |> to_list |> List.map decode_rootp |> Array.of_list in
    Poly.from_array coeffs

  let decode_frac json =
    let num_json = json |> member "num" in
    let den_json = json |> member "den" in
    let num = decode_poly num_json in
    let den = decode_poly den_json in
    Frac.v (num, den)

  (* Exp method term: coeff (fraction), exponent (poly) *)
  let decode_term_exp json =
    let coeff_json = json |> member "coeff" in
    let exp_json = json |> member "exponent" in
    {
      coeff = decode_frac coeff_json;
      exponent = decode_poly exp_json;
    }

  (* Log method term: coeff (poly), argument (poly) *)
  let decode_term_log json =
    let coeff_json = json |> member "coeff" in
    let arg_json = json |> member "argument" in
    (decode_poly coeff_json, decode_poly arg_json)

  let decode_input json =
    let m_str = json |> member "method" |> to_string_option |> Option.value ~default:"exp" in
    let terms_list = json |> member "terms" |> to_list in
    match m_str with
    | "exp" -> Exp (List.map decode_term_exp terms_list)
    | "log" -> Log (List.map decode_term_log terms_list)
    | _ -> failwith ("Unknown integral method: " ^ m_str)

  let needs_parens s =
    let s = String.trim s in
    if s = "" then false else
    let has_plus = String.contains s '+' in
    let has_minus = 
      try let i = String.index s '-' in i > 0
      with Not_found -> false 
    in
    has_plus || has_minus

  let terms_to_latex_exp terms =
    let parts = List.map (fun {coeff; exponent} -> 
      let c_str = Frac.to_string_latex coeff in
      let e_str = Poly.to_string_latex exponent in
      let e_part = if e_str = "0" then "" else if e_str = "1" then "e^x" else Printf.sprintf "e^{%s}" e_str in
      if c_str = "1" then (if e_part = "" then "1" else e_part)
      else if e_part = "" then c_str
      else if needs_parens c_str then Printf.sprintf "\\left( %s \\right)%s" c_str e_part
      else c_str ^ e_part
    ) terms in
    String.concat " + " parts

  let terms_to_latex_log terms =
    let parts = List.map (fun (c, arg) ->
      let c_str = Poly.to_string_latex c in
      let a_str = Poly.to_string_latex arg in
      let c_part = 
        if c_str = "1" then "" 
        else if c_str = "-1" then "-"
        else if needs_parens c_str then Printf.sprintf "\\left( %s \\right) " c_str
        else c_str ^ " "
      in
      Printf.sprintf "%s\\log\\left( %s \\right)" c_part a_str
    ) terms in
    String.concat " + " parts

  let run () =
    try
      while true do
        let line = read_line () |> String.trim in
        if line <> "" then
          try
            let json_str =
              if line.[0] = '{' then
                line
              else
                let ic = open_in line in
                let content = really_input_string ic (in_channel_length ic) in
                close_in ic;
                content
            in
            let json = Yojson.Safe.from_string json_str in
            
            match decode_input json with
            | Exp terms ->
                let res = integrate terms in
                let full_tex =
                  if res.nonelementary <> [] then "\\text{non elementary}"
                  else to_string_latex res.elementary
                in
                let integrand = terms_to_latex_exp terms in
                print_full_latex (wrap_equation (Printf.sprintf "\\int \\left( %s \\right) dx = %s" integrand full_tex))
            
            | Log terms ->
                let res = LogInt.integrate_expr terms in
                let full_tex = LogInt.to_string_latex res in
                let integrand = terms_to_latex_log terms in
                print_full_latex (wrap_equation (Printf.sprintf "\\int \\left( %s \\right) dx = %s" integrand full_tex))

          with e ->
            print_full_latex (Printf.sprintf "Error: %s" (Printexc.to_string e))
      done
    with End_of_file -> ()
end

(* Entry Point *)
let () =
  if Array.length Sys.argv < 2 then
    failwith "Usage: main <settings_json_file>";
  
  let filename = Sys.argv.(1) in
  let ic = open_in filename in
  let settings_str = really_input_string ic (in_channel_length ic) in
  close_in ic;

  let settings_json = Yojson.Safe.from_string settings_str in
  
  let mode_str =
    try settings_json |> member "mode" |> to_string
    with _ -> "Unknown"
  in

  match mode_str with
  | "LinearE" -> LinearERunner.run ()
  | "LinearDE" -> LinearDE.run ()
  | "Integral" | "integral" -> Integral.run ()
  | _ -> failwith ("Unsupported mode: " ^ mode_str)

