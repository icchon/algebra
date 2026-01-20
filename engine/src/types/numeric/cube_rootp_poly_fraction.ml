(* Use Q_cube_rootp.CubicNumber as the coefficient field *)
module CubeRootP = struct
  include Q_cube_rootp.CubicNumber
end

module CubeRootP_polynomial = struct
  include Polynomial.Make(CubeRootP)
  let v x = x
  let from_array x = normalize x
end

module BASE = Fraction.Make(CubeRootP_polynomial)

include BASE

let to_string_latex x = BASE.to_string_latex x
let derive x = BASE.derive x

let of_crootp r = BASE.v (CubeRootP_polynomial.from_array [| r |], CubeRootP_polynomial.one)

let of_int_poly coeffs =
  let c_coeffs = Array.map (fun i -> CubeRootP.make_rational (i, 1)) coeffs in
  let p = CubeRootP_polynomial.from_array c_coeffs in
  BASE.v (p, CubeRootP_polynomial.one)

let integrate_poly p =
  let len = Array.length p in
  if len = 0 then CubeRootP_polynomial.zero
  else
    let res = Array.make (len + 1) CubeRootP.zero in
    for i = 0 to len - 1 do
      res.(i + 1) <- CubeRootP.mul p.(i) (CubeRootP.inv (CubeRootP.make_rational (i + 1, 1)))
    done;
    CubeRootP_polynomial.normalize res

type integration_result = {
  rational_part : t;
  log_part : (CubeRootP.t * CubeRootP_polynomial.t) list;
  remaining_square_free : t;
}

let split x =
  let p, q = x in
  let s = CubeRootP_polynomial.quo p q in
  let r = CubeRootP_polynomial.rem p q in
  (s, (r, q))

let square_free_factors p =
  let g = CubeRootP_polynomial.gcd p (CubeRootP_polynomial.derive p) in
  if CubeRootP_polynomial.degree g <= 0 then [p]
  else
    let v = CubeRootP_polynomial.quo p g in
    let w = CubeRootP_polynomial.quo (CubeRootP_polynomial.derive p) g in
    let rec aux v w acc =
      if CubeRootP_polynomial.degree v <= 0 then List.rev acc
      else
        let y = CubeRootP_polynomial.sub w (CubeRootP_polynomial.derive v) in
        let d = CubeRootP_polynomial.gcd v y in
        aux
          (CubeRootP_polynomial.quo v d)
          (CubeRootP_polynomial.quo y d)
          (d :: acc)
    in
    aux v w []

let rec reduce_power a q k r_poly =
  if k <= 1 then (BASE.zero, a)
  else
    let q_deriv = CubeRootP_polynomial.derive q in
    let rhs =
      CubeRootP_polynomial.neg
        (CubeRootP_polynomial.mul (CubeRootP_polynomial.times r_poly (k - 1)) q_deriv)
    in
    let gcd_val, _s, t = CubeRootP_polynomial.gcd_ext q rhs in
    if CubeRootP_polynomial.degree gcd_val > 0 then
      failwith "Hermite reduction failed: gcd(q, rhs) is not constant";
    let inv_g = CubeRootP.inv gcd_val.(0) in
    let b =
      CubeRootP_polynomial.rem
        (CubeRootP_polynomial.mul
           (CubeRootP_polynomial.mul a t)
           (CubeRootP_polynomial.pure inv_g))
        q
    in
    let b_deriv = CubeRootP_polynomial.derive b in
    let term_to_sub =
      CubeRootP_polynomial.mul
        (CubeRootP_polynomial.sub
           (CubeRootP_polynomial.mul b_deriv q)
           (CubeRootP_polynomial.mul (CubeRootP_polynomial.times b (k - 1)) q_deriv))
        r_poly
    in
    let c =
      CubeRootP_polynomial.quo (CubeRootP_polynomial.sub a term_to_sub) q
    in
    let rat = BASE.v (b, CubeRootP_polynomial.pow q (k - 1)) in
    let next_rat, final_c = reduce_power c q (k - 1) r_poly in
    (BASE.add rat next_rat, final_c)

let hermite_reduction a d =
  let factors = square_free_factors d in
  let rec aux current_a =
    function
    | [] -> (BASE.zero, current_a, CubeRootP_polynomial.one)
    | [ d1 ] -> (BASE.zero, current_a, d1)
    | dm :: rest_rev ->
        let k = List.length rest_rev + 1 in
        let factors_rest = List.rev rest_rev in
        let r_poly =
          List.mapi
            (fun i di -> CubeRootP_polynomial.pow di (i + 1))
            factors_rest
          |> List.fold_left CubeRootP_polynomial.mul CubeRootP_polynomial.one
        in
        let rat, next_a = reduce_power current_a dm k r_poly in
        let next_rat, final_a, final_d = aux next_a rest_rev in
        (BASE.add rat next_rat, final_a, CubeRootP_polynomial.mul final_d dm)
  in
  let rat, rem_num, rem_den = aux a (List.rev factors) in
  (rat, (rem_num, rem_den))

let eval_poly p x =
  Array.fold_right (fun c acc -> CubeRootP.add c (CubeRootP.mul acc x)) p CubeRootP.zero

(* Helper to solve quadratic equation in CubeRootP *)
let solve_quadratic b c =
  (* x^2 + bx + c = 0 *)
  (* D = b^2 - 4c *)
  (* We need square root of CubicNumber. 
     Currently we only support square roots of Q_rootp.t if they are available. *)
    let disc = CubeRootP.sub (CubeRootP.mul b b) (CubeRootP.mul (CubeRootP.make_rational (4, 1)) c) in
    (* CubicNumber consists of Q_rootp.t terms. If it's a single Q_rootp.t term with no roots/omega... *)
    match Q_rootp.square_root (match Q_cube_rootp.CubicBasisMap.bindings disc.terms with
      | [(basis, q)] when Q_cube_rootp.IdMap.is_empty basis.roots && basis.omega = 0 -> q
      | _ -> failwith "Solving quadratic with general CubicNumber coefficients is not supported yet") with
    | Some s ->
        let s_c = CubeRootP.of_q_rootp s in
        let r1 = CubeRootP.mul (CubeRootP.add (CubeRootP.neg b) s_c) (CubeRootP.inv (CubeRootP.make_rational (2, 1))) in
        let r2 = CubeRootP.mul (CubeRootP.sub (CubeRootP.neg b) s_c) (CubeRootP.inv (CubeRootP.make_rational (2, 1))) in
        [r1; r2]
    | None -> failwith "Could not solve quadratic equation in CubeRootP (no square root found)"
  
  (* Cardano Solver for CubicNumber coefficients (specialized for rational case or simple CubicNumber) *)
  let solve_cubic b c d =
    (* x^3 + bx^2 + cx + d = 0 *)
    let one = Rational.one in
      let to_rat (x : CubeRootP.t) = 
        if CubeRootP.is_zero x then Rational.zero
        else match Q_cube_rootp.CubicBasisMap.bindings x.terms with
          | [(basis, q)] when Q_cube_rootp.IdMap.is_empty basis.roots && basis.omega = 0 -> 
              (match Q_rootp.to_rational_opt q with Some r -> r | None -> failwith "Not a rational")
          | _ -> failwith "Not a rational"
      in
    
    try
      let br = to_rat b in
      let cr = to_rat c in
      let dr = to_rat d in
      Cardano_solver.solve_cubic_raw one br cr dr
    with _ ->
      (* If not rational, we would need to implement Cardano in CubicNumber directly. 
         Let's try a bit. *)
      let p = CubeRootP.sub c (CubeRootP.mul (CubeRootP.mul b b) (CubeRootP.inv (CubeRootP.make_rational (3, 1)))) in
      let q = CubeRootP.add (CubeRootP.sub (CubeRootP.mul (CubeRootP.mul (CubeRootP.mul b b) b) (CubeRootP.make_rational (2, 27)))
                 (CubeRootP.mul (CubeRootP.mul b c) (CubeRootP.make_rational (1, 3))))
                 d
      in
      let disc = CubeRootP.add (CubeRootP.mul (CubeRootP.mul q q) (CubeRootP.make_rational (1, 4)))
                    (CubeRootP.mul (CubeRootP.mul (CubeRootP.mul p p) p) (CubeRootP.make_rational (1, 27)))
      in
      (* sqrt(disc) *)
      let sqrt_disc = match Q_rootp.square_root (match Q_cube_rootp.CubicBasisMap.bindings disc.terms with
        | [(basis, q)] when Q_cube_rootp.IdMap.is_empty basis.roots && basis.omega = 0 -> q
        | _ -> failwith "General cubic root not supported") with
      | Some s -> CubeRootP.of_q_rootp s
      | None -> failwith "No square root of discriminant"
      in
      let u_inner = CubeRootP.add (CubeRootP.neg (CubeRootP.mul q (CubeRootP.make_rational (1, 2)))) sqrt_disc in
      let u = CubeRootP.make_root3 (match Q_cube_rootp.CubicBasisMap.bindings u_inner.terms with
        | [(basis, q)] when Q_cube_rootp.IdMap.is_empty basis.roots && basis.omega = 0 -> q
        | _ -> failwith "General cubic root not supported") in
      let v = if CubeRootP.is_zero u then CubeRootP.zero 
              else CubeRootP.neg (CubeRootP.mul p (CubeRootP.inv (CubeRootP.mul (CubeRootP.make_rational (3, 1)) u))) in
      let omega = CubeRootP.omega in
      let omega2 = CubeRootP.mul omega omega in
      let r1 = CubeRootP.add u v in
      let r2 = CubeRootP.add (CubeRootP.mul omega u) (CubeRootP.mul omega2 v) in
      let r3 = CubeRootP.add (CubeRootP.mul omega2 u) (CubeRootP.mul omega v) in
      let shift = CubeRootP.mul b (CubeRootP.make_rational (1, 3)) in
      [CubeRootP.sub r1 shift; CubeRootP.sub r2 shift; CubeRootP.sub r3 shift]
  
let rec log_part_integration_of_factor a d =
  let deg_d = CubeRootP_polynomial.degree d in
  if deg_d <= 0 then []
  else if deg_d = 1 then
    let c1 = d.(1) in
    let c0 = d.(0) in
    let a0 = if Array.length a > 0 then a.(0) else CubeRootP.zero in
    let coeff = CubeRootP.div a0 c1 in
    let log_arg =
      CubeRootP_polynomial.from_array [| CubeRootP.div c0 c1; CubeRootP.one |]
    in
    [ (coeff, log_arg) ]
  else if deg_d = 2 then
    let c2 = d.(2) in
    let c1 = d.(1) in
    let c0 = d.(0) in
    let b = CubeRootP.div c1 c2 in
    let c = CubeRootP.div c0 c2 in
    let a_norm = Array.map (fun x -> CubeRootP.div x c2) a in
    let roots = solve_quadratic b c in
    match roots with
    | [ r1; r2 ] ->
        let diff = CubeRootP.sub r1 r2 in
        let coeff1 = CubeRootP.div (eval_poly a_norm r1) diff in
        let coeff2 = CubeRootP.div (eval_poly a_norm r2) (CubeRootP.neg diff) in
        [
          (coeff1, CubeRootP_polynomial.from_array [| CubeRootP.neg r1; CubeRootP.one |]);
          (coeff2, CubeRootP_polynomial.from_array [| CubeRootP.neg r2; CubeRootP.one |]);
        ]
    | _ -> failwith "Quadratic should have 2 roots"
  else if deg_d = 3 then
    let c3 = d.(3) in
    let c2 = d.(2) in
    let c1 = d.(1) in
    let c0 = d.(0) in
    let b = CubeRootP.div c2 c3 in
    let c = CubeRootP.div c1 c3 in
    let d_val = CubeRootP.div c0 c3 in
    let a_norm = Array.map (fun x -> CubeRootP.div x c3) a in
    let roots = solve_cubic b c d_val in
    let rec aux_roots rs acc =
      match rs with
      | [] -> acc
      | ri :: rest ->
          let other_roots =
            List.filter (fun r -> not (CubeRootP.equal r ri)) roots
          in
          let denom =
            List.fold_left
              (fun acc rj -> CubeRootP.mul acc (CubeRootP.sub ri rj))
              CubeRootP.one other_roots
          in
          let coeff = CubeRootP.div (eval_poly a_norm ri) denom in
          let log_arg =
            CubeRootP_polynomial.from_array [| CubeRootP.neg ri; CubeRootP.one |]
          in
          aux_roots rest ((coeff, log_arg) :: acc)
    in
    aux_roots roots []
  else
    let d_deriv = CubeRootP_polynomial.derive d in
    let g = CubeRootP_polynomial.gcd d d_deriv in
    if CubeRootP_polynomial.degree g > 0 then
       let factors = [g; CubeRootP_polynomial.quo d g] in
       log_part_integration_multi a factors
    else
       failwith ("Integration of log part for factor degree " ^ string_of_int deg_d ^ " not implemented")

and log_part_integration_multi a factors =
  match factors with
  | [] -> []
  | [ d ] -> log_part_integration_of_factor a d
  | d :: rest ->
      if CubeRootP_polynomial.degree d = 0 then log_part_integration_multi a rest
      else
        let other =
          List.fold_left CubeRootP_polynomial.mul CubeRootP_polynomial.one rest
        in
        if CubeRootP_polynomial.degree other = 0 then
          log_part_integration_of_factor a d
        else
          let g, s, t = CubeRootP_polynomial.gcd_ext d other in
          if CubeRootP_polynomial.degree g > 0 then
            failwith "Square-free factors are not coprime";
          let inv_g = CubeRootP.inv g.(0) in
          let a_t = CubeRootP_polynomial.mul a t in
          let a_s = CubeRootP_polynomial.mul a s in
          let term_d =
            CubeRootP_polynomial.mul a_t (CubeRootP_polynomial.pure inv_g)
          in
          let term_rest =
            CubeRootP_polynomial.mul a_s (CubeRootP_polynomial.pure inv_g)
          in
          log_part_integration_of_factor (CubeRootP_polynomial.rem term_d d) d
          @ log_part_integration_multi
              (CubeRootP_polynomial.rem term_rest other)
              rest

let log_part_integration a d =
  if CubeRootP_polynomial.degree a < 0 then []
  else
    let factors =
      List.filter
        (fun f -> CubeRootP_polynomial.degree f > 0)
        (square_free_factors d)
    in
    log_part_integration_multi a factors
let integrate_with_factors x factors =
  let s, (r, q) = split x in
  let s_int_poly = integrate_poly s in
  let rat_part_from_poly = BASE.v (s_int_poly, CubeRootP_polynomial.one) in
  let rat_part_from_hermite, (rem_num, _rem_den) = hermite_reduction r q in
  let logs = log_part_integration_multi rem_num factors in
  {
    rational_part = BASE.add rat_part_from_poly rat_part_from_hermite;
    log_part = logs;
    remaining_square_free = (rem_num, q);
  }

let integrate x =
  let _p, q = x in
  let factors =
    List.filter
      (fun f -> CubeRootP_polynomial.degree f > 0)
      (square_free_factors q)
  in
  integrate_with_factors x factors
