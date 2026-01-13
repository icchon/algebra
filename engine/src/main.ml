open Yojson.Safe.Util 
module StringMap = Map.Make(String)

type pure_scalar = 
  | Rational
  | Quadratic
  | Cubic
  | Quaternion
  | Octonion
  | Sedenion
  | Polynomial
  | PolynomialFraction

type element =
  | Scalar of pure_scalar
  | Matrix of element
  | Vector of element

type env = {
  main_element: element;
  sub_element: element option;
}

type unary_op = 
  | Neg | Inv

type binary_op =
  | Add | Sub | Mul | Div | Pow

type command = 
  | Define of {id: string; layer: int; value: Yojson.Safe.t}
  | UnaryOp of {id: string; op: unary_op; args: string}
  | BinaryOp of {id: string; op: binary_op; args: string * string }
  | Eval of {id: string; poly_id: string; target_id: string }
  | Print of {id: string}
  | Clear 


let decode_unary_op = function
  | "neg" -> Neg
  | "inv" -> Inv
  | s -> failwith ("Unknown unary operation: " ^ s)

let decode_binary_op = function
  | "add" -> Add
  | "sub" -> Sub
  | "mul" -> Mul
  | "div" -> Div
  | "pow" -> Pow
  | s -> failwith ("Unknown binary operation: " ^ s)

let decode_command json =
  match json |> member "action" |> to_string with
  | "define" ->
      Define {
        id = json |> member "id" |> to_string;
        layer = json |> member "layer" |> to_int;
        value = json |> member "value"
      }
  | "unary" ->
      UnaryOp {
        id = json |> member "id" |> to_string;
        op = json |> member "op" |> to_string |> decode_unary_op;
        args = json |> member "args" |> to_string
      }
  | "binary" ->
      let arg_list = json |> member "args" |> to_list in
      BinaryOp {
        id = json |> member "id" |> to_string;
        op = json |> member "op" |> to_string |> decode_binary_op;
        args = (to_string (List.nth arg_list 0), to_string (List.nth arg_list 1))
      }
  | "eval" ->
      Eval {
        id = json |> member "id" |> to_string;
        poly_id = json |> member "poly_id" |> to_string;
        target_id = json |> member "target_id" |> to_string
      }
  | "print" ->
      Print { id = json |> member "id" |> to_string }
  | "clear" ->
      Clear
  | action -> failwith ("Unknown action: " ^ action)

let rec decode_element json = 
    let t = json |> member "type" |> to_string in
    match t with
    | "Matrix" -> Matrix (decode_element (member "element" json))
    | "Vector" -> Vector (decode_element (member "element" json))
    | "Rational" -> Scalar Rational
    | "Quadratic" -> Scalar Quadratic
    | "Cubic" -> Scalar Cubic
    | "Quaternion" -> Scalar Quaternion
    | "Octonion" -> Scalar Octonion
    | "Sedenion" -> Scalar Sedenion
    | "Polynomial" -> Scalar Polynomial
    | "PolynomialFraction" -> Scalar PolynomialFraction
    | _ -> failwith ("Unkonw type: " ^ t)

(* let rec element_to_string = function
  | Scalar Rational           -> "Rational"
  | Scalar Quadratic          -> "Quadratic"
  | Scalar Cubic              -> "Cubic"
  | Scalar Quaternion         -> "Quaternion"
  | Scalar Octonion           -> "Octonion"
  | Scalar Sedenion           -> "Sedenion"
  | Scalar Polynomial         -> "Polynomial"
  | Scalar PolynomialFraction -> "PolynomialFraction"
  | Matrix e -> Printf.sprintf "Matrix(%s)" (element_to_string e)
  | Vector e -> Printf.sprintf "Vector(%s)" (element_to_string e) *)

let decode_env json = 
  let layers = json |> member "layers" |> to_list in
  {
    main_element = decode_element (List.nth layers 0);
    sub_element = if List.length layers > 1 then Some (decode_element (List.nth layers 1)) else None;
  }

let parse_env_str json_str = 
  Yojson.Safe.from_string json_str |> decode_env

let parse_command_str json_str = 
  Yojson.Safe.from_string json_str |> decode_command



type ('m, 's) engine_state = {
  main_vars : 'm StringMap.t;
  sub_vars : 's StringMap.t;
}

module type ALGEBRA = sig
  type t
  val of_yojson: Yojson.Safe.t -> t
  val to_string_latex : t -> string
  val to_string: t -> string
  val add : t -> t -> t
  val sub: t -> t -> t
  val mul : t -> t -> t
  val inv : t -> t
end

type ('m, 's) algebra_pkg = {
  main_alg : (module ALGEBRA with type t = 'm);
  sub_alg  : (module ALGEBRA with type t = 's) option;
}
type any_algebra = 
  | Alg: (module ALGEBRA with type t = 'a) -> any_algebra

let get_algebra_from_element element : any_algebra = match element with
  | Scalar Rational -> Alg (module Types.Rational)
  | _ -> Alg (module Types.Rational)


let step (type m s) (pkg : (m, s) algebra_pkg) (state : (m, s) engine_state) cmd =
  match cmd with
  | Define { id; layer; value } ->
      if layer = 0 then
        let (module M) = pkg.main_alg in
        let v = M.of_yojson value in
        ({ state with main_vars = StringMap.add id v state.main_vars }, "Defined " ^ id)
      else
        (match pkg.sub_alg with
         | Some (module S) ->
             let v = S.of_yojson value in
             ({ state with sub_vars = StringMap.add id v state.sub_vars }, "Defined " ^ id)
         | None -> failwith "No sub layer defined")

  | BinaryOp { id; op; args = (l, r) } ->
      let (module M) = pkg.main_alg in
      let v1 = StringMap.find l state.main_vars in
      let v2 = StringMap.find r state.main_vars in
      let res = match op with
        | Add -> M.add v1 v2
        | Sub -> M.sub v1 v2
        | Mul -> M.mul v1 v2
        | _ -> failwith "Op not supported yet"
      in
      ({ state with main_vars = StringMap.add id res state.main_vars }, M.to_string_latex res)

  | Print { id } ->
      let (module M) = pkg.main_alg in
      let v = StringMap.find id state.main_vars in
      (state, M.to_string_latex v)

  | _ -> (state, "Command not fully implemented")


let run (type m s) (pkg : (m, s) algebra_pkg) =
  let state = ref { main_vars = StringMap.empty; sub_vars = StringMap.empty } in
  
  let execute_line current_state line =
    let line = String.trim line in
    if line = "" then current_state
    else
      if line.[0] = '{' then
        try
          let cmd = parse_command_str line in
          let (next_state, tex) = step pkg current_state cmd in
          Printf.printf "{\"latex\": \"%s\"}\n" tex;
          flush stdout;
          next_state
        with e -> 
          Printf.printf "{\"error\": \"JSON: %s\"}\n" (Printexc.to_string e);
          current_state
      else
      try
        let ic = open_in line in
        let content = really_input_string ic (in_channel_length ic) in
        close_in ic;
        
        let cmd = parse_command_str content in
        let (next_state, tex) = step pkg current_state cmd in
        Printf.printf "{\"latex\": \"%s\"}\n" tex;
        flush stdout;
        next_state
      with e ->
        Printf.printf "{\"error\": \"File processing error (%s): %s\"}\n" line (Printexc.to_string e);
        flush stdout;
        current_state
  in

  try
    while true do
      state := execute_line !state (read_line ())
    done
  with End_of_file -> ()
  
let () =
  let config = parse_env_str Sys.argv.(1) in
  let Alg (module Main) = get_algebra_from_element config.main_element in
  
  match config.sub_element with
  | Some sub_el ->
      let Alg (module Sub) = get_algebra_from_element sub_el in
      run { main_alg = (module Main); sub_alg = Some (module Sub) }
  | None ->
      let pkg = { main_alg = (module Main); sub_alg = None } in
      run pkg