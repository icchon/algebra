open Q_cube_rootp
module Q = Q_rootp
module C = CubicNumber
module Rat = Rational

(* Helper for Quadratic: ax^2 + bx + c = 0 *)
let solve_quadratic_raw a b c =
  let disc = Rat.sub (Rat.mul b b) (Rat.mul (4, 1) (Rat.mul a c)) in
  let s_disc = Q.make_root_rat disc in
  let r1 = Q.div (Q.add (Q.make_rational (Rat.neg b)) s_disc) (Q.make_rational (Rat.mul (2, 1) a)) in
  let r2 = Q.div (Q.sub (Q.make_rational (Rat.neg b)) s_disc) (Q.make_rational (Rat.mul (2, 1) a)) in
  List.map C.of_q_rootp [ r1; r2 ]

(* Helper for Linear: ax + b = 0 *)
let solve_linear_raw a b =
  [ C.of_q_rootp (Q.make_rational (Rat.div (Rat.neg b) a)) ]

let solve_cubic_raw a b c d =
  if Rat.is_zero a then
    if Rat.is_zero b then
      if Rat.is_zero c then [] (* Constant case, no roots *)
      else solve_linear_raw c d
    else solve_quadratic_raw b c d
  else
    let div_rat x y = Rat.mul x (Rat.inv y) in
    let p = div_rat (Rat.sub (Rat.mul (3, 1) (Rat.mul a c)) (Rat.mul b b)) (Rat.mul (3, 1) (Rat.mul a a)) in
    let q = div_rat (Rat.add (Rat.mul (2, 1) (Rat.mul b (Rat.mul b b)))
               (Rat.sub (Rat.mul (27, 1) (Rat.mul a (Rat.mul a d)))
                  (Rat.mul (9, 1) (Rat.mul a (Rat.mul b c)))))
               (Rat.mul (27, 1) (Rat.mul a (Rat.mul a a)))
    in
    let disc_rat = Rat.add (Rat.mul (Rat.mul q q) (1, 4)) (Rat.mul (Rat.mul (Rat.mul p p) p) (1, 27)) in
    let sqrt_d = Q.make_root_rat disc_rat in
    let minus_q_over_2 = Q.make_rational (Rat.neg (Rat.mul q (1, 2))) in
    
    let u_inner = Q.add minus_q_over_2 sqrt_d in
    let u = if Q.is_zero u_inner then C.make_root3 (Q.add minus_q_over_2 (Q.neg sqrt_d))
            else C.make_root3 u_inner in
    let v = if C.is_zero u then C.zero else
            let p_c = C.of_q_rootp (Q.make_rational p) in
            C.neg (C.div p_c (C.mul (C.of_q_rootp (Q.make_rational (3, 1))) u)) in
    
    let omega = C.omega in
    let omega2 = C.mul omega omega in
    let y1 = C.add u v in
    let y2 = C.add (C.mul omega u) (C.mul omega2 v) in
    let y3 = C.add (C.mul omega2 u) (C.mul omega v) in
    let shift = C.of_q_rootp (Q.make_rational (div_rat b (Rat.mul (3, 1) a))) in
    [ C.sub y1 shift; C.sub y2 shift; C.sub y3 shift ]