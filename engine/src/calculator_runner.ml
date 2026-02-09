open Types
open Printf
open Yojson.Safe.Util

module type RUNNER = sig
  val run : unit -> unit
end

module Make (A : Calculator_field.S) = struct
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

  let run () =
    try
      while true do 
        match read_json_from_input () with 
        | Some json -> 
          (try 
            let expression_json = member "expression" json in 
            let expr = A.decode_expression expression_json in 
            let result = A.eval expr in 
            let lhs_latex = A.to_string_latex expr in 
            let rhs_latex = A.to_string_latex result in 
            let equation = sprintf "%s = %s" lhs_latex rhs_latex in 
            print_full_latex (wrap_align equation)
          with e -> 
            print_full_latex (Printf.sprintf "Error: %s" (Printexc.to_string e)))
        | None -> raise End_of_file
      done
    with End_of_file -> ()
end