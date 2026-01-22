open Yojson.Safe.Util
open Path_resolver
open Unix

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

let read_json_from_input () =
  if isatty stdin then
    (* Interactive mode: read line by line *)
    let line = read_line () |> String.trim in
    if line = "" then None
    else
      if line.[0] = '{' then
        Some (Yojson.Safe.from_string line)
      else
        let ic = open_in (resolve_path line) in
        let content = really_input_string ic (in_channel_length ic) in
        close_in ic;
        Some (Yojson.Safe.from_string content)
  else
    (* Piped input: read one line from stdin *)
    try
      let line = read_line () in
      if String.trim line = "" then None
      else Some (Yojson.Safe.from_string line)
    with End_of_file -> None

let smart_join parts =
  let filtered = List.filter (fun s -> let s = String.trim s in s <> "" && s <> "0") parts in
  match filtered with
  | [] -> "0"
  | hd :: tl ->
    let hd = String.trim hd in
    List.fold_left (fun acc part ->
      let part = String.trim part in
      if String.length part > 0 && part.[0] = '-' then
        acc ^ " - " ^ (String.sub part 1 (String.length part - 1))
      else
        acc ^ " + " ^ part
    ) hd tl

let polish_latex str =
  let str = Str.global_replace (Str.regexp " \\+ -") " - " str in
  let str = Str.global_replace (Str.regexp " - -") " + " str in
  str

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
        match read_json_from_input () with
        | Some json ->
          (try
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
            print_full_latex (Printf.sprintf "Error: %s" (Printexc.to_string e)))
        | None -> raise End_of_file
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
    if valid = [] then "0" else
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
    if terms = [] then "0" else smart_join terms

  let run () =
    try
      while true do
        match read_json_from_input () with
        | Some json ->
          (try
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
            print_full_latex (Printf.sprintf "Error: %s" (Printexc.to_string e)))
        | None -> raise End_of_file
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

  type integral_term =
    | IExp of Types.Rootp_frac_exp.term
    | ILog of (Frac.t * Poly.t)

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

  let decode_input json : integral_term list =
    let decode_one_term term_json =
      if member "exponent" term_json <> `Null then
        IExp {
          coeff = term_json |> member "coeff" |> decode_frac;
          exponent = term_json |> member "exponent" |> decode_poly;
        }
      else if member "log" term_json <> `Null then
        ILog (
          term_json |> member "coeff" |> decode_frac,
          term_json |> member "log" |> decode_poly
        )
      else if member "coeff" term_json <> `Null then
        IExp {
          coeff = term_json |> member "coeff" |> decode_frac;
          exponent = Poly.zero
        }
      else failwith "unknown integral term"
    in
    json |> member "terms" |> to_list |> List.map decode_one_term

  let needs_parens s =
    let s = String.trim s in
    if s = "" then false else
    let has_plus = String.contains s '+' in
    let has_minus = 
      try let i = String.index s '-' in i > 0
      with Not_found -> false 
    in
    has_plus || has_minus

  let terms_to_latex terms =
    let parts = List.map (function
      | IExp {coeff; exponent} ->
          let c_str = Frac.to_string_latex coeff in
          let e_str = Poly.to_string_latex exponent in
          let e_part = if e_str = "0" then "" else if e_str = "1" then "e^x" else Printf.sprintf "e^{%s}" e_str in
          if c_str = "1" then (if e_part = "" then "1" else e_part)
          else if e_part = "" then c_str
          else if needs_parens c_str then Printf.sprintf "\\left( %s \\right)%s" c_str e_part
          else c_str ^ e_part
      | ILog (c, arg) ->
          let c_str = Frac.to_string_latex c in
          let a_str = Poly.to_string_latex arg in
          let c_part = 
            if c_str = "1" then "" 
            else if c_str = "-1" then "-"
            else if needs_parens c_str then Printf.sprintf "\\left( %s \\right) " c_str
            else c_str ^ " "
          in
          Printf.sprintf "%s\\log\\left( %s \\right)" c_part a_str
    ) terms in
    smart_join parts

  let run () =
    try
      while true do
        match read_json_from_input () with
        | Some json ->
          (try
            let terms = decode_input json in
            
            let rat_coeffs = terms |> List.filter_map (function IExp {coeff; exponent} when Poly.is_zero exponent -> Some coeff | _ -> None) in
            let exp_terms = terms |> List.filter_map (function IExp t when not (Poly.is_zero t.exponent) -> Some t | _ -> None) in
            let log_terms_frac = terms |> List.filter_map (function ILog t -> Some t | _ -> None) in

            let total_rat_func = List.fold_left Frac.add Frac.zero rat_coeffs in
            let rat_res = Frac.integrate total_rat_func in
            
            let exp_res = integrate exp_terms in
            
            let poly_log_terms = log_terms_frac |> List.map (fun (c, arg) ->
              let (num, den) = c in
              if not (Poly.equal den Poly.one) then
                failwith "Log term with rational function coefficient not supported yet"
              else (num, arg)
            ) in
            let log_res = LogInt.integrate_expr poly_log_terms in

            let tex_parts = ref [] in
            let add_part s = if s <> "" && s <> "0" then tex_parts := s :: !tex_parts in
            
            add_part (Frac.to_string_latex rat_res.rational_part);
            add_part (Frac.to_string_latex log_res.rational_part);
            add_part (terms_to_latex (List.map (fun t -> IExp t) exp_res.elementary));
            
            let combined_log_part = LogInt.{
              rational_part=Frac.zero; 
              log_part=rat_res.log_part @ log_res.log_part; 
              poly_log_part=log_res.poly_log_part
            } in
            add_part (LogInt.to_string_latex combined_log_part);

            let non_elem_integrand =
              (if exp_res.nonelementary <> [] then
                let exp_integrand = List.map (fun t -> IExp t) exp_res.nonelementary in
                [terms_to_latex exp_integrand]
              else []) @
              (if not (Frac.is_zero rat_res.remaining_square_free) then 
                [Frac.to_string_latex rat_res.remaining_square_free] 
              else [])
            in
            
            let final_tex_parts = List.rev !tex_parts in

            let elementary_part_multiline = match final_tex_parts with
            | [] -> ""
            | hd :: tl ->
                let rest = tl |> List.map (fun s ->
                    if String.length s > 0 && s.[0] = '-' then
                        "& " ^ s
                    else
                        "& + " ^ s
                ) |> String.concat " \\\\ " in
                if rest = "" then hd else hd ^ " \\\\ " ^ rest
            in
            
            let full_tex =
              let elementary_part = if elementary_part_multiline = "" then "0" else elementary_part_multiline in
              if non_elem_integrand <> [] then
                let integrand_str = String.concat " + " non_elem_integrand in
                if elementary_part = "0" then Printf.sprintf "\\int %s dx" integrand_str
                else elementary_part ^ " \\\\ & + \\int " ^ integrand_str ^ " dx"
              else
                elementary_part
            in
            
            let integrand = polish_latex (terms_to_latex terms) in
            let equation = Printf.sprintf "\\int \\left( %s \\right) dx &= %s" integrand full_tex in
            print_full_latex (wrap_align (polish_latex equation))
          with e ->
            print_full_latex (Printf.sprintf "Error: %s" (Printexc.to_string e)))
        | None -> raise End_of_file
      done
    with End_of_file -> ()
end

(* Entry Point *)
let () =
  if Array.length Sys.argv < 2 then
    failwith "Usage: main <settings_json_file>";
  
  let filename = resolve_path Sys.argv.(1) in
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
