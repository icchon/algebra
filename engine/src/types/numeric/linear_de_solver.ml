module Poly = Rootp_poly_fraction.RootP_polynomial
module RootP = Rootp_poly_fraction.RootP
module C = Q_cube_rootp.CubicNumber
module Rat = Rational

type solution_term = { poly : Poly.t; alpha : C.t }
type solution = solution_term list
type equation = { coeffs : RootP.t array; forcing : solution }

let c_of_p p = C.of_q_rootp p
let is_minus_one_c c = C.equal c (C.neg C.one)
let is_minus_one_p p = Poly.equal p (Poly.neg Poly.one)

let format_alpha a =
  if C.is_one a then "" else if is_minus_one_c a then "-" else C.to_string_latex a

let to_string_latex_term { poly; alpha } =
  let p_s = Poly.to_string_latex poly in
  if C.is_zero alpha then p_s
  else
    let a_s = format_alpha alpha in
    let p_part =
      if Poly.is_one poly then "" else if is_minus_one_p poly then "-"
      else 
        let needs_parens = 
          let s = String.trim p_s in
          (try let i = String.index s '+' in i > 0 with Not_found -> false) ||
          (try let i = String.index s '-' in i > 0 with Not_found -> false)
        in
        if needs_parens then {| \left( |} ^ p_s ^ {| \right) |}
        else p_s
    in p_part ^ "e^{ " ^ a_s ^ " x}"

let join_terms terms =
  let rec aux is_first =
    function
    | [] -> ""
    | t :: ts ->
        if t = "0" then aux is_first ts
        else
          let op = if is_first then "" else if t.[0] = '-' then " - " else " + " in
          let s = if not is_first && t.[0] = '-' then String.sub t 1 (String.length t - 1) else t in
          op ^ s ^ aux false ts
  in let res = aux true terms in if res = "" then "0" else res

let to_string_latex sol = join_terms (List.map to_string_latex_term sol)

(* General root finding for degree 1 to 3 *)

let get_roots coeffs =
  let rec get_actual_coeffs =
    function
    | [] -> []
    | c :: cs -> if RootP.is_zero c then get_actual_coeffs cs else c :: cs
  in
  let actual = List.rev (get_actual_coeffs (List.rev (Array.to_list coeffs))) |> Array.of_list in
  let n = Array.length actual - 1 in
  let to_rat r = match RootP.to_rational_opt r with Some q -> q | None -> failwith "Rational coeffs required" in
  if n <= 0 then []
  else if n = 1 then [C.div (c_of_p (RootP.neg actual.(0))) (c_of_p actual.(1))]
  else if n = 2 then
    let a, b, c = actual.(2), actual.(1), actual.(0) in
    let disc = RootP.sub (RootP.mul b b) (RootP.mul (RootP.of_int 4) (RootP.mul a c)) in
    match RootP.square_root disc with
    | Some s ->
        let r1 = C.div (c_of_p (RootP.add (RootP.neg b) s)) (c_of_p (RootP.mul (RootP.of_int 2) a)) in
        let r2 = C.div (c_of_p (RootP.sub (RootP.neg b) s)) (c_of_p (RootP.mul (RootP.of_int 2) a)) in [r1; r2]
    | None -> failwith "Quadratic disc not solvable"
  else if n = 3 then Cardano_solver.solve_cubic_raw (to_rat actual.(3)) (to_rat actual.(2)) (to_rat actual.(1)) (to_rat actual.(0))
  else failwith "Order > 3 not supported"

let solve_homogeneous coeffs =
  let roots = get_roots coeffs in
  let rec group acc =
    function
    | [] -> acc
    | r :: rs -> let same, others = List.partition (fun x -> C.equal x r) rs in group ((r, List.length same + 1) :: acc) others
  in
  let groups = group [] roots in
  List.concat_map (fun (r, mult) ->
    List.init mult (fun k ->
      let p = Array.make (k + 1) RootP.zero in p.(k) <- RootP.one;
      { poly = Poly.from_array p; alpha = r }
    )
  ) groups

module CP = struct
  let derive p = let n = Array.length p in if n <= 1 then [||] else Array.init (n-1) (fun i -> C.mul p.(i+1) (C.of_q_rootp (RootP.of_int (i+1))))
  let add p1 p2 = let l1, l2 = Array.length p1, Array.length p2 in Array.init (max l1 l2) (fun i -> C.add (if i < l1 then p1.(i) else C.zero) (if i < l2 then p2.(i) else C.zero))
  let scale c p = Array.map (C.mul c) p
end

let rec combinations n r = if r = 0 || r = n then 1 else combinations (n - 1) (r - 1) + combinations (n - 1) r

let solve_for_q coeffs p_poly alpha mult =
  let n_order = Array.length coeffs - 1 in
  let a_c = Array.map c_of_p coeffs in
  let apply_la q =
    let res = ref [||] in
    for j = 0 to n_order do
      if not (C.is_zero a_c.(j)) then
        let term_j = ref q in
        for _ = 1 to j do term_j := CP.add (CP.derive !term_j) (CP.scale alpha !term_j) done;
        res := CP.add !res (CP.scale a_c.(j) !term_j)
    done; !res
  in
  let deg_p = Array.length p_poly - 1 in
  let deg_q = deg_p + mult in
  let q_coeffs = Array.make (deg_q + 1) C.zero in
  let current_p = ref (Array.map c_of_p p_poly) in
  let fact m = let r = ref 1 in for k = 1 to m do r := !r * k done; !r in
  let falling_fact m k = let r = ref 1 in for j = 0 to k - 1 do r := !r * (m - j) done; !r in
  let get_lowest_operator_coeff m =
    let sum = ref C.zero in
    for j = m to n_order do
      let term = C.mul a_c.(j) (C.mul (C.of_q_rootp (RootP.of_int (combinations j m))) (C.pow alpha (j-m))) in
      sum := C.add !sum term
    done; !sum
  in
  let op_coeff = get_lowest_operator_coeff mult in
  for i = deg_q downto mult do
    let target_deg = i - mult in
    let lead_p = if target_deg >= 0 && target_deg < Array.length !current_p then (!current_p).(target_deg) else C.zero in
    let current_factor = C.mul op_coeff (C.of_q_rootp (RootP.of_int (falling_fact i mult / fact mult))) in
    let qi = C.div lead_p current_factor in
    q_coeffs.(i) <- qi;
    let qi_xi = Array.make (i + 1) C.zero in qi_xi.(i) <- qi;
    current_p := CP.add !current_p (Array.map C.neg (apply_la qi_xi))
  done; q_coeffs

let solve { coeffs; forcing } =
  let homogeneous = solve_homogeneous coeffs in
  let roots = get_roots coeffs in
  let particular = List.map (fun { poly; alpha } ->
    let mult = List.filter (fun r -> C.equal r alpha) roots |> List.length in
    let q_coeffs = solve_for_q coeffs poly alpha mult in (q_coeffs, alpha)
  ) forcing in (homogeneous, particular)

let format_poly_c qs var_str =
  let items = Array.mapi (fun i c ->
    if C.is_zero c then "" else
    let c_s = C.to_string_latex c in
    let b_s = if i=0 then "" else if i=1 then "x" else "x^{ " ^ string_of_int i ^ " }" in
    if b_s = "" then c_s else if C.is_one c then b_s else if is_minus_one_c c then "-" ^ b_s
    else if String.contains c_s '+' || (String.contains c_s '-' && String.index c_s '-' > 0) then {| \left( |} ^ c_s ^ {| \right) |} ^ b_s else c_s ^ b_s
  ) qs |> Array.to_list |> List.filter ((<>) "") in
  let poly_s = join_terms items in
  if var_str = "" then poly_s
  else if String.contains poly_s '+' || (String.contains poly_s '-' && String.index poly_s '-' > 0) then {| \left( |} ^ poly_s ^ {| \right) |} ^ var_str
  else if poly_s = "1" then var_str else if poly_s = "-1" then "-" ^ var_str else poly_s ^ var_str

let to_string_latex_full (homogeneous, particular) =
  let h_terms = List.mapi (fun i t ->
    let term_s = to_string_latex_term t in
    let c_name = Printf.sprintf "C_{ %d }" (i + 1) in
    if term_s = "1" then c_name else c_name ^ term_s
  ) homogeneous in
  let h_s = join_terms h_terms in
    let p_s_list = List.map (fun (qs, alpha) -> format_poly_c qs (if C.is_zero alpha then "" else "e^{" ^ format_alpha alpha ^ " x}")) particular in
    join_terms (h_s :: p_s_list)
  
  (* JSON Decoding *)
  let solution_term_of_yojson json =
    let open Yojson.Safe.Util in
    let p_json = json |> member "poly" in
    let alpha_json = json |> member "alpha" in
    {
      poly = Poly.from_array (p_json |> to_list |> List.map (fun j -> RootP.of_int (to_int j)) |> Array.of_list);
      alpha = C.of_q_rootp (RootP.of_int (to_int alpha_json));
    }
  
  let equation_of_yojson coeffs_json forcing_json =
    let open Yojson.Safe.Util in
    {
      coeffs = coeffs_json |> to_list |> List.map (fun j -> RootP.of_int (to_int j)) |> Array.of_list;
      forcing = forcing_json |> to_list |> List.map solution_term_of_yojson;
    }
  