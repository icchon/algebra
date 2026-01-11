open Types
module R = Latex_reporter
module CS = Cardano_solver
module C = Q_cube_rootp.CubicNumber
module Q = Q_rootp

(** PolynomialモジュールのためのQ_rootpアダプタ *)
module Q_coeff = struct
  include Q

  let to_string_latex_level _ x = to_string_latex x
  let is_one x = equal x one

  let is_negative x =
    let s = to_string_latex x in
    String.length s > 0 && s.[0] = '-'
end

module P = Polynomial.Make (Q_coeff)
(** 多項式モジュールのインスタンス化 *)

let test_case title (an, ad) (bn, bd) (cn, cd) (dn, dd) =
  R.section title;

  (* 係数を生成 *)
  let qa = Q.make_rational (an, ad) in
  let qb = Q.make_rational (bn, bd) in
  let qc = Q.make_rational (cn, cd) in
  let qd = Q.make_rational (dn, dd) in

  (* 多項式オブジェクトの作成 [d; c; b; a] (低次から高次の順) *)
  let poly = [| qd; qc; qb; qa |] in

  (* Polynomialモジュールを使用して方程式を表示 *)
  let eq_str = P.to_string_latex poly in
  R.adds (R.eq eq_str "0");

  let sols = CS.solve_cubic_raw (an, ad) (bn, bd) (cn, cd) (dn, dd) in

  List.iteri
    (fun i sol ->
      let label = Printf.sprintf "x_{%d}" (i + 1) in
      R.adds (R.eq label (C.to_string_latex sol)))
    sols;

  R.subsection "Verification by Substitution";

  let ca = C.of_q_rootp qa in
  let cb = C.of_q_rootp qb in
  let cc = C.of_q_rootp qc in
  let cd = C.of_q_rootp qd in

  List.iteri
    (fun i x ->
      let x2 = C.mul x x in
      let x3 = C.mul x x2 in
      let res =
        C.add (C.mul ca x3) (C.add (C.mul cb x2) (C.add (C.mul cc x) cd))
      in
      let label = Printf.sprintf "f(x_{%d})" (i + 1) in
      R.adds (R.eq label (C.to_string_latex res)))
    sols

let test () =
  test_case "Test 1" (1, 1) (0, 1) (0, 1) (-1, 1);
  test_case "Test 2" (1, 1) (0, 1) (-3, 1) (2, 1);
  test_case "Test 3" (1, 1) (1, 1) (1, 1) (1, 1);
  test_case "Test 4" (1, 1) (-6, 1) (11, 1) (-6, 1);
  test_case "Test 5" (1, 1) (-6, 1) (12, 1) (-8, 1);
  test_case "Test 6" (1, 1) (0, 1) (-3, 1) (2, 1);
  test_case "Test 7" (1, 1) (0, 1) (-7, 1) (-6, 1);
  test_case "Test 8" (1, 1) (-2, 1) (4, 1) (-8, 1);
  test_case "Test 9" (1, 1) (-3, 1) (4, 1) (-2, 1);
  test_case "Test 10" (1, 1) (0, 1) (0, 1) (-2, 1);
  test_case "Test 11" (4, 1) (-8, 1) (-1, 1) (2, 1);
  test_case "Test 12" (1, 1) (0, 1) (-3, 1) (-1, 1);
  test_case "Test 13" (1, 1) (0, 1) (-700, 1) (6000, 1)

let () =
  test ();
  print_string (R.get_full_latex_document ())
