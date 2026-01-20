(* Use Q_rootp.DynamicNumber as the coefficient field *)
module RootP = struct
  include Q_rootp.DynamicNumber
  let to_string_latex_level _ x = to_string_latex x
  let is_negative = is_negative
  let compare = compare
end

module RootP_polynomial = struct
  include Polynomial.Make(RootP)
  let v x = x
  let from_array x = normalize x
end

module BASE = Fraction.Make(RootP_polynomial)

include BASE

(* Extended functionality required for Base module *)

let to_string_latex x = BASE.to_string_latex x

let derive x = BASE.derive x

let of_rootp r = BASE.v (RootP_polynomial.from_array [| r |], RootP_polynomial.one)

(* Helper to create simple polynomial in x with integer coefficients (converted to RootP) *)
let of_int_poly coeffs =
  let rootp_coeffs = Array.map RootP.of_int coeffs in
  let p = RootP_polynomial.from_array rootp_coeffs in
  BASE.v (p, RootP_polynomial.one)

let integrate_poly p =
  let len = Array.length p in
  if len = 0 then RootP_polynomial.zero
  else
    let res = Array.make (len + 1) RootP.zero in
    for i = 0 to len - 1 do
      res.(i + 1) <- RootP.div p.(i) (RootP.of_int (i + 1))
    done;
    RootP_polynomial.normalize res

type integration_result = {
  rational_part : t;
  log_part : (RootP.t * RootP_polynomial.t) list;
  remaining_square_free : t;
}

let split x =
  let p, q = x in
  let s = RootP_polynomial.quo p q in
  let r = RootP_polynomial.rem p q in
  (s, (r, q))

let square_free_factors p =
  let g = RootP_polynomial.gcd p (RootP_polynomial.derive p) in
  if RootP_polynomial.degree g <= 0 then [p]
  else
    let v = RootP_polynomial.quo p g in
    let w = RootP_polynomial.quo (RootP_polynomial.derive p) g in
    let rec aux v w acc =
      if RootP_polynomial.degree v <= 0 then List.rev acc
      else
        let y = RootP_polynomial.sub w (RootP_polynomial.derive v) in
        let d = RootP_polynomial.gcd v y in
        aux
          (RootP_polynomial.quo v d)
          (RootP_polynomial.quo y d)
          (d :: acc)
    in
    aux v w []

let rec reduce_power a q k r_poly =
  if k <= 1 then (BASE.zero, a)
  else
    let q_deriv = RootP_polynomial.derive q in
    let rhs =
      RootP_polynomial.neg
        (RootP_polynomial.mul (RootP_polynomial.times r_poly (k - 1)) q_deriv)
    in
    let gcd_val, _s, t = RootP_polynomial.gcd_ext q rhs in
    if RootP_polynomial.degree gcd_val > 0 then
      failwith "Hermite reduction failed: gcd(q, rhs) is not constant";
    let inv_g = RootP.inv gcd_val.(0) in
    let b =
      RootP_polynomial.rem
        (RootP_polynomial.mul
           (RootP_polynomial.mul a t)
           (RootP_polynomial.pure inv_g))
        q
    in
    let b_deriv = RootP_polynomial.derive b in
    let term_to_sub =
      RootP_polynomial.mul
        (RootP_polynomial.sub
           (RootP_polynomial.mul b_deriv q)
           (RootP_polynomial.mul (RootP_polynomial.times b (k - 1)) q_deriv))
        r_poly
    in
    let c =
      RootP_polynomial.quo (RootP_polynomial.sub a term_to_sub) q
    in
    let rat = BASE.v (b, RootP_polynomial.pow q (k - 1)) in
    let next_rat, final_c = reduce_power c q (k - 1) r_poly in
    (BASE.add rat next_rat, final_c)

let hermite_reduction a d =
  let factors = square_free_factors d in
  let rec aux current_a = function
    | [] -> (BASE.zero, current_a, RootP_polynomial.one)
    | [ d1 ] -> (BASE.zero, current_a, d1)
    | dm :: rest_rev ->
        let k = List.length rest_rev + 1 in
        let factors_rest = List.rev rest_rev in
        let r_poly =
          List.mapi
            (fun i di -> RootP_polynomial.pow di (i + 1))
            factors_rest
          |> List.fold_left RootP_polynomial.mul RootP_polynomial.one
        in
        let rat, next_a = reduce_power current_a dm k r_poly in
        let next_rat, final_a, final_d = aux next_a rest_rev in
        (BASE.add rat next_rat, final_a, RootP_polynomial.mul final_d dm)
  in
  let rat, rem_num, rem_den = aux a (List.rev factors) in
  (rat, (rem_num, rem_den))

let eval_poly p x =
  Array.fold_right (fun c acc -> RootP.add c (RootP.mul acc x)) p RootP.zero

let log_part_integration a d =
  if RootP_polynomial.degree a < 0 then []
  else
    let deg_d = RootP_polynomial.degree d in
    if deg_d <= 0 then []
    else if deg_d = 1 then
      (* a / (c1*x + c0) = (a/c1) / (x + c0/c1) -> (a/c1) * log(x + c0/c1) *)
      let c1 = d.(1) in
      let c0 = d.(0) in
      let a0 = if Array.length a > 0 then a.(0) else RootP.zero in
      let coeff = RootP.div a0 c1 in
      let log_arg =
        RootP_polynomial.from_array [| RootP.div c0 c1; RootP.one |]
      in
      [ (coeff, log_arg) ]
    else if deg_d = 2 then
      (* a / (c2*x^2 + c1*x + c0) *)
      let c2 = d.(2) in
      let c1 = d.(1) in
      let c0 = d.(0) in
      (* Normalize d to x^2 + (c1/c2)x + (c0/c2) *)
      let b = RootP.div c1 c2 in
      let c = RootP.div c0 c2 in
      (* Normalize a as well *)
      let a_norm = Array.map (fun x -> RootP.div x c2) a in
      (* Roots of x^2 + bx + c = 0: x = (-b +/- sqrt(b^2 - 4c)) / 2 *)
      let disc = RootP.sub (RootP.mul b b) (RootP.mul (RootP.of_int 4) c) in
      match RootP.square_root disc with
      | Some sqrt_d ->
          let r1 =
            RootP.div (RootP.add (RootP.neg b) sqrt_d) (RootP.of_int 2)
          in
          let r2 =
            RootP.div (RootP.sub (RootP.neg b) sqrt_d) (RootP.of_int 2)
          in
          if RootP.equal r1 r2 then
            failwith "Unexpected non-square-free denominator in log_part"
          else
            (* a(x) / ((x - r1)(x - r2)) = A / (x - r1) + B / (x - r2) *)
            (* A = a(r1) / (r1 - r2), B = a(r2) / (r2 - r1) *)
            let diff = RootP.sub r1 r2 in
            let coeff1 = RootP.div (eval_poly a_norm r1) diff in
            let coeff2 = RootP.div (eval_poly a_norm r2) (RootP.neg diff) in
            [
              (coeff1, RootP_polynomial.from_array [| RootP.neg r1; RootP.one |]);
              (coeff2, RootP_polynomial.from_array [| RootP.neg r2; RootP.one |]);
            ]
      | None -> failwith "Could not find square root of discriminant in RootP"
    else failwith "Integration of log part for degree > 2 not implemented"

let integrate x =
  let s, (r, q) = split x in
  let s_int_poly = integrate_poly s in
  let rat_part_from_poly = BASE.v (s_int_poly, RootP_polynomial.one) in
  let rat_part_from_hermite, (rem_num, rem_den) = hermite_reduction r q in
  let logs = log_part_integration rem_num rem_den in
  {
    rational_part = BASE.add rat_part_from_poly rat_part_from_hermite;
    log_part = logs;
    remaining_square_free = (rem_num, rem_den);
  }
