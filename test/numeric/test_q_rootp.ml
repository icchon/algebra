open Types
module R = Latex_reporter
module S = Q_rootp
open Printf

let test () =
  let ts = S.to_string_latex in
  let one = S.one in
  let of_i = S.of_int in

  (* --- Section 1: 独立した5つの平方根による多重拡大 (次元 2^5 = 32) --- *)
  R.section "Multi-Radical Arithmetic";

  let p_list = [ 2; 3; 5; 7; 11 ] in
  let roots = List.map S.make_root p_list in
  let sum_roots = List.fold_left S.add S.zero roots in

  (* (sqrt(2) + sqrt(3) + sqrt(5) + sqrt(7) + sqrt(11))^2 の展開 *)
  let sum_sq = S.mul sum_roots sum_roots in
  R.subsection "Square of sum of 5 radicals";
  R.adds (R.eq (R.pow (R.group (ts sum_roots)) "2") (ts sum_sq));

  (* --- Section 2: 虚数単位 i と根の混成演算 --- *)
  R.section "Complex and Radical Arithmetic";

  let i = S.make_root (-1) in
  let sqrt2 = S.make_root 2 in
  let sqrt3 = S.make_root 3 in

  (* (i + sqrt(2)) * (i + sqrt(3)) *)
  let expr1 = S.add i sqrt2 in
  let expr2 = S.add i sqrt3 in
  let prod_complex = S.mul expr1 expr2 in

  R.subsection "Complex radical product";
  R.adds
    (R.eq (R.mul (R.group (ts expr1)) (R.group (ts expr2))) (ts prod_complex));

  (* --- Section 3: 局所化された高次元逆元計算 (有理化) --- *)
  R.section "Localized Inversion Performance";

  (* 1 / (1 + i + sqrt(2) + sqrt(5)) *)
  (* 以前の実装なら全次元を相手にしていたが、今は必要な4つの基底による16次元空間で計算 *)
  let denom = S.add (S.add one i) (S.add sqrt2 (S.make_root 5)) in
  let inv_val = S.inv denom in

  R.subsection "Inversion of (1 + i + sqrt(2) + sqrt(5))";
  R.adds (R.eq (R.inv (R.group (ts denom))) (ts inv_val));

  (* 検証: 分母 * 逆元 = 1 *)
  let identity = S.mul denom inv_val in
  R.subsection "Verification: Result must be 1";
  R.adds
    (R.eq (R.mul (R.group (ts denom)) (R.group (ts inv_val))) (ts identity));

  (* --- Section 4: 零化と簡約の極限 --- *)
  R.section "Cancellation and Reduction";

  (* (sqrt(2) + sqrt(3) + sqrt(5)) * (sqrt(2) + sqrt(3) - sqrt(5)) *)
  (* = (sqrt(2) + sqrt(3))^2 - 5 = 2 + 3 + 2*sqrt(6) - 5 = 2*sqrt(6) *)
  let sqrt5 = S.make_root 5 in
  let base_sum = S.add sqrt2 sqrt3 in
  let factor1 = S.add base_sum sqrt5 in
  let factor2 = S.sub base_sum sqrt5 in
  let res_mul = S.mul factor1 factor2 in

  R.subsection "Product leading to reduction to sqrt(6)";
  R.adds
    (R.eq (R.mul (R.group (ts factor1)) (R.group (ts factor2))) (ts res_mul));

  (* --- Section 5: 大量定数倍と除算 --- *)
  R.section "Scaling and Division";

  (* (1024 * i) / (2 * sqrt(2)) = 256 * i * sqrt(2) *)
  let num = S.mul (of_i 1024) i in
  let den = S.mul (of_i 2) sqrt2 in
  let res_div = S.mul num (S.inv den) in

  R.subsection "Division of complex scaled terms";
  let expr_div = sprintf "%s \\div %s" (ts num) (ts den) in
  R.adds (R.eq expr_div (ts res_div))

let () =
  test ();
  print_string (R.get_full_latex_document ())
