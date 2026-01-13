open Types
module R = Latex_reporter
module C = Q_cube_rootp.CubicNumber
module Q = Q_rootp

let test () =
  let ts = C.to_string_latex in
  (* let one = C.one in *)
  let of_q = C.of_q_rootp in

  (* --- Section 1: 複雑な地体 (Layer 1) の準備 --- *)
  print_endline "Section 1";
  R.section "Deep Layer 1 Base Field";

  (* 地体 Q(i, sqrt(2), sqrt(3), sqrt(5)) の要素を作成 *)
  let i = Q.make_root (-1) in
  let sqrt2 = Q.make_root 2 in
  let sqrt3 = Q.make_root 3 in
  let sqrt5 = Q.make_root 5 in

  (* 地体の複雑な数 alpha = (i + sqrt(2)) / (sqrt(3) + sqrt(5)) *)
  let alpha_q = Q.mul (Q.add i sqrt2) (Q.inv (Q.add sqrt3 sqrt5)) in
  let alpha = of_q alpha_q in
  R.subsection "Element $\\alpha$ from complex Layer 1";
  R.adds (R.eq "\\alpha" (ts alpha));

  (* --- Section 2: 複雑な地体上の立方根 --- *)
  print_endline "Section 2";
  R.section "Cubic Roots over Complex Base";

  (* u = root3(alpha) *)
  let u = C.make_root3 alpha_q in
  let u3 = C.mul u (C.mul u u) in

  R.subsection "Verification: $u = \\sqrt[3]{\\alpha}$";
  R.adds (R.eq (R.pow "u" "3") (ts u3));

  (* --- Section 3: 多重立方根と地体の混成 --- *)
  print_endline "Section 3";
  R.section "Mixed Nested Arithmetic";

  (* v = root3(sqrt(5)) *)
  let v = C.make_root3 sqrt5 in

  (* sum = alpha + u + v *)
  let sum_val = C.add alpha (C.add u v) in
  let sq_val = C.mul sum_val sum_val in

  R.subsection
    "Expansion of $(\\alpha + \\sqrt[3]{\\alpha} + \\sqrt[3]{\\sqrt{5}})^2$";
  R.adds (R.eq (R.pow (R.group (ts sum_val)) "2") (ts sq_val));

  (* --- Section 4: 局所化された極限の逆元計算 --- *)
  print_endline "Section 4";
  R.section "Extreme Localized Inversion";

  (* denom = i + sqrt(2) + root3(2) + root3(3) *)
  let c_i = of_q i in
  let c_sqrt2 = of_q sqrt2 in
  let u_root2 = C.make_root3 (Q.of_int 2) in
  let v_root3 = C.make_root3 (Q.of_int 3) in

  let denom = C.add (C.add c_i c_sqrt2) (C.add u_root2 v_root3) in
  print_endline "Before inv";
  let inv_val = C.inv denom in
  print_endline "After inv";

  R.subsection
    "Rationalization of $1 / (i + \\sqrt{2} + \\sqrt[3]{2} + \\sqrt[3]{3})$";
  R.adds (R.eq (R.inv (R.group (ts denom))) (ts inv_val));

  (* 検証: denom * inv_val = 1 *)
  let check_id = C.mul denom inv_val in
  R.subsection "Identity Verification";
  R.adds
    (R.eq (R.mul (R.group (ts denom)) (R.group (ts inv_val))) (ts check_id));

  (* --- Section 5: 高度な簡約 (カルダノ公式の構成要素) --- *)
  print_endline "Section 5";
  R.section "Cardano Structural Test";

  (* 地体の w = (sqrt(2) + i*sqrt(6)) *)
  let w_q = Q.add sqrt2 (Q.mul i (Q.make_root 6)) in
  let u_cardano = C.make_root3 w_q in

  (* (u + 1/u) のような構造の演算 *)
  let inv_u = C.inv u_cardano in
  let expr = C.add u_cardano inv_u in
  let cube_expr = C.mul expr (C.mul expr expr) in

  R.subsection "Simplification of $(u + u^{-1})^3$";
  R.adds (R.eq (R.pow (R.group (ts expr)) "3") (ts cube_expr))

let () =
  test ();
  print_string (R.get_full_latex_document ())
