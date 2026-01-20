module Frac = Rootp_poly_fraction
module Poly = Frac.RootP_polynomial
module RootP = Frac.RootP

type term = {
  coeff : Frac.t;    (* f(x) in K(x) *)
  exponent : Poly.t; (* P(x) in K[x] *)
}

type t = term list

let to_string_latex expr = 
  if expr = [] then "0"
  else
    let term_to_str { coeff; exponent } = 
      let c_str = Frac.to_string_latex coeff in
      let e_str = Poly.to_string_latex exponent in
      let c_part = 
        if c_str = "1" then ""
        else if c_str = "-1" then "-"
        else 
          let needs_parens = 
            let s = String.trim c_str in
            String.contains s '+' || (try let i = String.index s '-' in i > 0 with Not_found -> false)
          in
          if needs_parens then {| \left( |} ^ c_str ^ {| \right) |}
          else c_str
      in
      c_part ^ "e^{ " ^ e_str ^ "}"
    in 
    String.concat " + " (List.map term_to_str expr)

(* Solve Y' * R + Q * Y = S for polynomial Y *)
let solve_risch_extended s q r = 
  let deg_s = Poly.degree s in
  let deg_q = Poly.degree q in
  if deg_s < 0 then Some Poly.zero
  else 
    (* deg(Q*Y) = deg(Q) + deg(Y).
       deg(Y'*R) = deg(Y) - 1 + deg(R).
       For non-constant P, deg(Q) = deg(P'B) = deg(P') + deg(B) is high enough.
       So deg(Y) = deg(S) - deg(Q). *)
    let deg_y = deg_s - deg_q in 
    if deg_y < 0 then None
    else 
      let y_coeffs = Array.make (deg_y + 1) RootP.zero in
      let current_s = ref s in
      let lead_q = q.(deg_q) in
      
      let rec solve i = 
        if i < 0 then 
          if Poly.degree !current_s < 0 then Some (Poly.normalize y_coeffs)
          else None
        else 
          let deg_cur = Poly.degree !current_s in 
          if deg_cur < deg_q + i then (
            y_coeffs.(i) <- RootP.zero;
            solve (i - 1)
          ) else if deg_cur > deg_q + i then None
          else 
            let yi = RootP.div (!current_s).(deg_cur) lead_q in 
            y_coeffs.(i) <- yi;
            let yi_xi = Array.make (i + 1) RootP.zero in 
            yi_xi.(i) <- yi;
            let term_to_sub = Poly.add (Poly.mul (Poly.derive yi_xi) r) (Poly.mul q yi_xi) in 
            current_s := Poly.sub !current_s term_to_sub;
            solve (i - 1)
      in 
      solve deg_y

type integration_result = {
  elementary : t;
  nonelementary : t;
}

let integrate expr = 
  let elem = ref [] in
  let non_elem = ref [] in 
  List.iter (fun { coeff; exponent } -> 
    let a, b = coeff in 
    let p_deriv = Poly.derive exponent in 
    (* Equation: Y'B + (P'B - B')Y = AB *)
    let r = b in 
    let q = Poly.sub (Poly.mul p_deriv b) (Poly.derive b) in 
    let s = Poly.mul a b in 
    
    match solve_risch_extended s q r with 
    | Some y -> 
        let res_coeff = Frac.v (y, b) in 
        elem := { coeff = res_coeff; exponent } :: !elem
    | None -> non_elem := { coeff; exponent } :: !non_elem
  ) expr;
  { elementary = List.rev !elem; nonelementary = List.rev !non_elem }