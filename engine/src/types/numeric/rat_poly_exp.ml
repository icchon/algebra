module Poly = Rat_polynomial
module Rat = Rational

type term = {
  coeff : Poly.t;    (* A(x) *)
  exponent : Poly.t; (* P(x) in exp(P(x)) *)
}

type t = term list

let to_string_latex expr =
  if expr = [] then "0"
  else
    let term_to_str { coeff; exponent } =
      let c_str = Poly.to_string_latex coeff in
      let e_str = Poly.to_string_latex exponent in
      let c_part = 
        if c_str = "1" then "" 
        else if c_str = "-1" then "-"
        else 
          let needs_parens = 
            let s = String.trim c_str in
            (try let i = String.index s '+' in i > 0 with Not_found -> false) ||
            (try let i = String.index s '-' in i > 0 with Not_found -> false)
          in
          if needs_parens then {| \left( |} ^ c_str ^ {| \right) |}
          else c_str
      in
      c_part ^ "e^{" ^ e_str ^ "}"
    in 
    String.concat " + " (List.map term_to_str expr)

(* Solve B' + Q*B = A for polynomial B *)
let solve_risch_poly a q = 
  let deg_a = Poly.degree a in
  let deg_q = Poly.degree q in
  if deg_a < 0 then Some Poly.zero
  else if deg_q < 0 then
    (* B' = A, pure polynomial integration *)
    let len = Array.length a in
    let res = Array.make (len + 1) Rat.zero in
    for i = 0 to len - 1 do
      res.(i+1) <- Rat.div a.(i) (Rat.of_int (i+1))
    done;
    Some (Poly.normalize res)
  else
    (* B' + QB = A. 
       If deg(Q) >= 0, then deg(QB) = deg(Q) + deg(B).
       Since deg(B') < deg(B) <= deg(QB), QB must dominate the leading term.
       So deg(B) = deg(A) - deg(Q). *)
    let deg_b = deg_a - deg_q in 
    if deg_b < 0 then None
    else 
      let b_coeffs = Array.make (deg_b + 1) Rat.zero in
      let current_a = ref a in
      let lead_q = q.(deg_q) in
      
      let rec solve i = 
        if i < 0 then 
          if Poly.degree !current_a < 0 then Some (Poly.normalize b_coeffs)
          else None (* No polynomial solution *)
        else 
          let deg_cur = Poly.degree !current_a in 
          if deg_cur < deg_q + i then (
            (* Leading term of QB would be at deg_q + i. If A has smaller degree, b_i must be 0 *)
            b_coeffs.(i) <- Rat.zero;
            solve (i - 1)
          ) else if deg_cur > deg_q + i then (
            None (* A has a degree too high to be matched by QB and B' *)
          ) else 
            let b_i = Rat.div (!current_a).(deg_cur) lead_q in 
            b_coeffs.(i) <- b_i;
            (* Update current_a: A = A - ( (b_i x^i)' + Q * (b_i x^i) ) *)
            let bi_xi = Array.make (i + 1) Rat.zero in 
            bi_xi.(i) <- b_i;
            let bi_xi_deriv = Poly.derive bi_xi in 
            let q_bi_xi = Poly.mul q bi_xi in 
            current_a := Poly.sub !current_a (Poly.add bi_xi_deriv q_bi_xi);
            solve (i - 1)
      in 
      solve deg_b

type integration_result = {
  elementary : t;
  nonelementary : t;
}

let integrate expr = 
  let elem = ref [] in 
  let non_elem = ref [] in 
  List.iter (fun { coeff; exponent } -> 
    let q = Poly.derive exponent in 
    match solve_risch_poly coeff q with 
    | Some b -> elem := { coeff = b; exponent } :: !elem
    | None -> non_elem := { coeff; exponent } :: !non_elem
  ) expr;
  { elementary = List.rev !elem; nonelementary = List.rev !non_elem }
