open Q_cube_rootp
module Q = Q_rootp
module C = CubicNumber
module Rat = Rational

let solve_cubic_raw a b c d =
  let div_rat x y = Rat.mul x (Rat.inv y) in

  let p =
    div_rat
      (Rat.sub (Rat.mul (3, 1) (Rat.mul a c)) (Rat.mul b b))
      (Rat.mul (3, 1) (Rat.mul a a))
  in

  let q =
    div_rat
      (Rat.add
         (Rat.mul (2, 1) (Rat.mul b (Rat.mul b b)))
         (Rat.sub
            (Rat.mul (27, 1) (Rat.mul a (Rat.mul a d)))
            (Rat.mul (9, 1) (Rat.mul a (Rat.mul b c)))))
      (Rat.mul (27, 1) (Rat.mul a (Rat.mul a a)))
  in

  let q_over_2 = Rat.mul q (1, 2) in
  let p_over_3 = Rat.mul p (1, 3) in

  let disc_rat =
    Rat.add
      (Rat.mul q_over_2 q_over_2)
      (Rat.mul p_over_3 (Rat.mul p_over_3 p_over_3))
  in

  let sqrt_d = Q.make_root_rat disc_rat in
  let minus_q_over_2 = Q.make_rational (Rat.neg q_over_2) in

  let u_inner = Q.add minus_q_over_2 sqrt_d in
  let u = C.make_root3 u_inner in

  (* 5. 1の原始3乗根 omega *)
  (* 自分で計算せず、CubicNumber 内で定義した基底を直接呼ぶ *)
  let omega = C.omega in

  (* omega2 も CubicNumber 側の定義を使うか、omega * omega で作る *)
  (* 基底同士の積なので、自動的に omega 指数が 2 (または -1-omega) になります *)
  let omega2 = C.mul omega omega in

  let p_c = C.of_q_rootp (Q.make_rational p) in

  let three = C.of_q_rootp (Q.make_rational (3, 1)) in
  let three_u = C.mul three u in

  let v = if C.is_zero u then C.zero else C.neg (C.mul p_c (C.inv three_u)) in

  let y1 = C.add u v in
  let y2 = C.add (C.mul omega u) (C.mul omega2 v) in
  let y3 = C.add (C.mul omega2 u) (C.mul omega v) in

  let b_3a = div_rat b (Rat.mul (3, 1) a) in
  let shift = C.of_q_rootp (Q.make_rational b_3a) in

  List.map (fun y -> C.sub y shift) [ y1; y2; y3 ]
