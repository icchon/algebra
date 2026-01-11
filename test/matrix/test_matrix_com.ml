(* open Types
module R = Latex_reporter

module D2 : Dim.S = struct
  let n = 2
end

module D3 : Dim.S = struct
  let n = 3
end

module M = Rectangular.Make (Complex) (D2) (D3)

let r = Rational.v
let c n_re d_re n_im d_im = Complex.v (r (n_re, d_re), r (n_im, d_im))
let mat rows = Array.of_list (List.map Array.of_list rows)

let test () =
  let ts = M.to_string_latex in

  (* 1. 複素長方形行列 A (2x3) *)
  let a =
    mat
      [
        [ c 1 1 1 1; c 2 1 0 1; c 3 1 (-1) 1 ];
        [ c 4 1 0 1; c 5 1 2 1; c 6 1 0 1 ];
      ]
  in

  (* 2. 随伴行列 (conj) のテスト *)
  let a_star = M.conj a in
  (* R.star を使うことで自動的に適切な括弧/中括弧が付与される *)
  let a_star_latex = R.star (ts a) in
  R.eq a_star_latex (ts a_star) |> R.adds;

  (* 3. 行列の積 A * A* *)
  let res_mul = M.mul a a_star in
  R.eq (R.mul (ts a) a_star_latex) (ts res_mul) |> R.adds;

  (* 4. スカラー倍 i * A *)
  let k = c 0 1 1 1 in
  (* i *)
  let ka = M.mul_scalar k a in
  R.eq (R.mul_scalar (k |> Complex.to_string_latex) (ts a)) (ts ka) |> R.adds;

  let v1 = mat [ [ c 1 1 1 1 ]; [ c 0 1 1 1 ] ] in
  (* v1 = [1+i, i]^T *)
  let v2 = mat [ [ c 2 1 0 1 ]; [ c 1 1 (-1) 1 ] ] in
  (* v2 = [2, 1-i]^T *)

  (* 2. 内積の計算: <v1, v2> = v1* * v2 *)
  (* M.conj は転置+共役なので、(1x2) * (2x1) -> (1x1) となる *)
  let dot_product = M.dot v1 v2 in
  R.eq (R.dot (ts v1) (ts v2)) (Complex.to_string_latex dot_product) |> R.adds;

  (* 3. ノルムの二乗の検証: <v1, v1> = ||v1||^2 *)
  let self_dot = M.dot v1 v1 in
  let n_sq = M.norm_sq v1 in
  R.eq (R.dot (ts v1) (ts v1)) (Complex.to_string_latex self_dot) |> R.adds;
  (* スカラーとしてのノルム表示 (Rationalの形式を想定) *)
  R.eq (R.norm_sq (ts v1)) (Complex.to_string_latex n_sq) |> R.adds;

  (* 5. 随伴行列の性質再確認 (行列 A * v) *)
  let a = mat [ [ c 1 1 0 1; c 2 1 0 1 ]; [ c 0 1 0 1; c 1 1 0 1 ] ] in
  let av = M.mul a v1 in
  R.eq (R.mul (ts a) (ts v1)) (ts av) |> R.adds

let () =
  test ();
  print_string (R.get_full_latex_document ()) *)
