open Types
module R = Latex_reporter
module S = Q_root23

(* Helper to create a rational number *)
let r n = Rational.of_int n

(* Helper to create a Q(sqrt(2), sqrt(3)) number
   a + b√2 + c√3 + d√6 *)
let q a b c d = [| r a; r b; r c; r d |]

let test () =
  let ts = S.to_string_latex in

  R.section "Q($\\sqrt{2}$, $\\sqrt{3}$)";

  (* ADD *)
  R.subsection "Addition";
  let x = q 1 1 0 0 in
  let y = q 3 0 1 0 in
  (* (1+√2) + (3+√3) = 4+√2+√3 *)
  let expected_add = q 4 1 1 0 in
  R.eq (R.add (ts x) (ts y)) (ts expected_add) |> R.adds;

  (* SUB *)
  R.subsection "Subtraction";
  let x = q 1 1 0 0 in
  let y = q 1 1 0 0 in
  (* (1+√2) - (1+√2) = 0 *)
  let expected_sub = S.zero in
  R.eq (R.sub (ts x) (ts y)) (ts expected_sub) |> R.adds;

  (* MUL *)
  R.subsection "Multiplication";
  (* (1+√2)(1-√2) = 1 - 2 = -1 *)
  let x = q 1 1 0 0 in
  let y = q 1 (-1) 0 0 in
  let expected_mul1 = q (-1) 0 0 0 in
  R.eq (R.mul (ts x) (ts y)) (ts expected_mul1) |> R.adds;

  (* (√2+√3)(√2-√3) = 2 - 3 = -1 *)
  let x = q 0 1 1 0 in
  let y = q 0 1 (-1) 0 in
  let expected_mul2 = q (-1) 0 0 0 in
  R.eq (R.mul (ts x) (ts y)) (ts expected_mul2) |> R.adds;

  (* INV *)
  R.subsection "Inversion";
  (* inv(1+√2) = -1+√2 *)
  let x = q 1 1 0 0 in
  let expected_inv = q (-1) 1 0 0 in
  R.eq (R.inv (ts x)) (ts expected_inv) |> R.adds;

  (* Check: x * inv(x) = 1 *)
  R.subsection "Inverse Property";
  let x = q 1 2 3 4 in
  let inv_x = S.inv x in
  R.eq (R.mul (ts x) (ts inv_x)) (ts S.one) |> R.adds;

  ()

let () =
  test ();
  print_string (R.get_full_latex_document ())
