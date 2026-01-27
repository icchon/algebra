open Types
open Printf
open Yojson.Safe.Util

module type RUNNER = sig
  val run : unit -> unit
end

module Make (A : Calculator_algebra.S) = struct
  (* These helpers are copied from main.ml and adapted *)
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
    Printf.sprintf "\\begin{align*}\n%s\n\\end{align*}\"" content

  let read_json_from_input () =
    if Unix.isatty Unix.stdin then
      let line = read_line () |> String.trim in
      if line = "" then None
      else
        if line.[0] = '{' then
          Some (Yojson.Safe.from_string line)
        else
          let ic = open_in (Path_resolver.resolve_path line) in
          let content = really_input_string ic (in_channel_length ic) in
          close_in ic;
          Some (Yojson.Safe.from_string content)
    else
      try
        let ic = Unix.in_channel_of_descr Unix.stdin in
        let seq = Yojson.Safe.seq_from_channel ic in
        match Seq.uncons seq with
        | Some (json, _) -> Some json
        | None -> None
      with _ -> None
  (* The actual calculator implementation *)
  type expression =
    | Value of A.t
    | Add of expression * expression
    | Sub of expression * expression
    | Mul of expression * expression
    | Div of expression * expression
    | Inv of expression
    | Neg of expression
    | Conj of expression

  (* 式をLaTeX文字列に変換する再帰関数 *)
  let rec expr_to_latex expr =
    let val_ts = A.to_string_latex in
    let paren s = sprintf "\\left( %s \\right)" s in
    let needs_paren e = match e with
      | Add _ | Sub _ -> true
      | Value q -> String.contains (val_ts q) ' '
      | _ -> false
    in

    match expr with 
    | Value q -> val_ts q
    | Add (l, r) -> sprintf "%s + %s" (expr_to_latex l) (expr_to_latex r)
    | Sub (l, r) -> 
        let r_str = if needs_paren r then paren (expr_to_latex r) else expr_to_latex r in 
        sprintf "%s - %s" (expr_to_latex l) r_str
    | Mul (l, r) -> 
        let l_str = if needs_paren l then paren (expr_to_latex l) else expr_to_latex l in 
        let r_str = if needs_paren r then paren (expr_to_latex r) else expr_to_latex r in 
        sprintf "%s \\cdot %s" l_str r_str
    | Div (l, r) -> 
        sprintf "\\frac{%s}{%s}" (expr_to_latex l) (expr_to_latex r)
    | Inv e -> sprintf "{%s}^-1" (paren (expr_to_latex e))
    | Neg e -> sprintf "-%s" (paren (expr_to_latex e))
    | Conj e -> sprintf "\\overline{%s}" (paren (expr_to_latex e))

  (* JSONから式をデコードする再帰関数 *)
  let rec decode_expression json =
    let op_member = member "op" json in
    if op_member <> `Null then begin
      let op_str = to_string op_member in
      match op_str with 
      | "add" -> 
        let lhs = decode_expression (member "lhs" json) in 
        let rhs = decode_expression (member "rhs" json) in 
        Add (lhs, rhs)
      | "sub" -> 
        let lhs = decode_expression (member "lhs" json) in 
        let rhs = decode_expression (member "rhs" json) in 
        Sub (lhs, rhs)
      | "mul" -> 
        let lhs = decode_expression (member "lhs" json) in 
        let rhs = decode_expression (member "rhs" json) in 
        Mul (lhs, rhs)
      | "div" -> 
        let lhs = decode_expression (member "lhs" json) in 
        let rhs = decode_expression (member "rhs" json) in 
        Div (lhs, rhs)
      | "inv" -> 
        let operand = decode_expression (member "operand" json) in 
        Inv operand
      | "neg" -> 
        let operand = decode_expression (member "operand" json) in 
        Neg operand
      | "conj" -> 
        let operand = decode_expression (member "operand" json) in 
        Conj operand
      | _ -> failwith ("Unsupported operator: " ^ op_str)
    end else begin
      (* The `type` field is now handled by the algebra-specific `decode_value` *)
      Value (A.decode_value (member "value" json))
    end

  (* 式を評価する再帰関数 *)
  let rec eval_expression expr =
    match expr with 
    | Value q -> q
    | Add (lhs, rhs) -> A.add (eval_expression lhs) (eval_expression rhs)
    | Sub (lhs, rhs) -> A.sub (eval_expression lhs) (eval_expression rhs)
    | Mul (lhs, rhs) -> A.mul (eval_expression lhs) (eval_expression rhs)
    | Div (lhs, rhs) -> A.div (eval_expression lhs) (eval_expression rhs)
    | Inv operand -> A.inv (eval_expression operand)
    | Neg operand -> A.neg (eval_expression operand)
    | Conj operand -> A.conj (eval_expression operand)

  let run () =
    try
      while true do 
        match read_json_from_input () with 
        | Some json -> 
          (try 
            let expression_json = member "expression" json in 
            let expr = decode_expression expression_json in 
            let result = eval_expression expr in 
            let lhs_latex = expr_to_latex expr in 
            let rhs_latex = A.to_string_latex result in 
            let equation = sprintf "%s = %s" lhs_latex rhs_latex in 
            print_full_latex (wrap_align equation)
          with e -> 
            print_full_latex (Printf.sprintf "Error: %s" (Printexc.to_string e)))
        | None -> raise End_of_file
      done
    with End_of_file -> ()
end