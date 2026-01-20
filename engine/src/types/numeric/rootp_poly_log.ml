module Frac = Rootp_poly_fraction
module Poly = Frac.RootP_polynomial
module RootP = Frac.RootP
open Printf

(* Represents: rational_part + sum(c_i * log(g_i)) + sum(P_j * log(Q_j)) *)
type t = {
  rational_part : Frac.t;
  log_part : (RootP.t * Poly.t) list;
  poly_log_part : (Poly.t * Poly.t) list;
}

let zero = {
  rational_part = Frac.zero;
  log_part = [];
  poly_log_part = [];
}

let to_string_latex expr =
  let r_str = Frac.to_string_latex expr.rational_part in
  let l_str = 
    List.map (fun (c, g) ->
      let c_str = RootP.to_string_latex c in
      let g_str = Poly.to_string_latex g in
      sprintf {|%s \log \left( %s \right)|} c_str g_str
    ) expr.log_part
  in
  let pl_str = 
    List.map (fun (p, q) ->
      let p_str = Poly.to_string_latex p in
      let q_str = Poly.to_string_latex q in
      let p_display = if String.contains p_str '+' || String.contains p_str '-' then "(" ^ p_str ^ ")" else p_str in
      sprintf {|%s \log \left( %s \right)|} p_display q_str
    ) expr.poly_log_part
  in
  let all = (if r_str = "0" then [] else [r_str]) @ l_str @ pl_str in
  if all = [] then "0" else String.concat " + " all

(* Integrate P(x) * log(Q(x)) *)
let integrate_term p q =
  let v_poly = Frac.integrate_poly p in (* V = \int P dx *)
  let q_deriv = Poly.derive q in
  (* W = (V * Q') / Q *)
  let w_num = Poly.mul v_poly q_deriv in
  let w_den = q in
  let w_frac = Frac.v (w_num, w_den) in
  let w_int = Frac.integrate w_frac in
  
  {
    rational_part = Frac.neg w_int.rational_part;
    log_part = List.map (fun (c, g) -> (RootP.neg c, g)) w_int.log_part;
    poly_log_part = [(v_poly, q)];
  }

let add a b =
  {
    rational_part = Frac.add a.rational_part b.rational_part;
    log_part = a.log_part @ b.log_part;
    poly_log_part = a.poly_log_part @ b.poly_log_part;
  }

let integrate_expr items =
  List.fold_left (fun acc (p, q) -> add acc (integrate_term p q)) zero items
