open Types
module R = Latex_reporter
module S = Q_root2

(* Helper to create a rational number *)
let r n d = (n, d)

(* Helper to create a Q_root2 number from two rational parts (a + b*sqrt(2)) *)
let qr a b =
  let poly_a = if a = (0, 1) then S.Coeff.zero else S.Coeff.v a in
  let poly_b = if b = (0, 1) then S.Coeff.zero else S.Coeff.v b in
  S.from_array [| poly_a; poly_b |]

let test () =
  let ts = S.to_string_latex in
  let r_ts = S.Coeff.to_string_latex in
  (* Rational.to_string_latex *)

  (* Define some Q_root2 constants *)
  let zero = S.zero in
  (* 0 *)
  let one = S.one in
  (* 1 *)
  let sqrt2 = qr (r 0 1) (r 1 1) in
  (* sqrt(2) *)
  let two = qr (r 2 1) (r 0 1) in
  (* 2 *)

  (* Test cases for basic constants *)
  R.eq (ts S.zero) (ts zero) |> R.adds;
  R.eq (ts S.one) (ts one) |> R.adds;

  (* Test cases for ADD *)
  let x = qr (r 1 1) (r 1 1) in
  (* 1 + sqrt(2) *)
  let y = qr (r 2 1) (r 3 1) in
  (* 2 + 3*sqrt(2) *)
  R.eq (R.add (ts x) (ts y)) (S.add x y |> ts) |> R.adds;
  R.eq (ts (S.add x y)) (ts (qr (r 3 1) (r 4 1))) |> R.adds;

  (* 3 + 4*sqrt(2) *)

  (* Test cases for SUB *)
  R.eq (R.sub (ts x) (ts y)) (S.sub x y |> ts) |> R.adds;
  R.eq (ts (S.sub x y)) (ts (qr (r (-1) 1) (r (-2) 1))) |> R.adds;

  (* -1 - 2*sqrt(2) *)

  (* Test cases for NEG *)
  R.eq (R.neg (ts x)) (S.neg x |> ts) |> R.adds;
  R.eq (ts (S.neg x)) (ts (qr (r (-1) 1) (r (-1) 1))) |> R.adds;

  (* -1 - sqrt(2) *)

  (* Test cases for MUL *)
  let x_mul = qr (r 1 1) (r 1 1) in
  (* 1 + sqrt(2) *)
  let y_mul = qr (r 2 1) (r 3 1) in
  (* 2 + 3*sqrt(2) *)
  (* (1+sqrt(2))(2+3*sqrt(2)) = 2 + 3*sqrt(2) + 2*sqrt(2) + 3*2 = 8 + 5*sqrt(2) *)
  R.eq (R.mul (ts x_mul) (ts y_mul)) (S.mul x_mul y_mul |> ts) |> R.adds;
  R.eq (ts (S.mul x_mul y_mul)) (ts (qr (r 8 1) (r 5 1))) |> R.adds;

  (* sqrt(2) * sqrt(2) = 2 *)
  R.eq (R.mul (ts sqrt2) (ts sqrt2)) (S.mul sqrt2 sqrt2 |> ts) |> R.adds;
  R.eq (ts (S.mul sqrt2 sqrt2)) (ts two) |> R.adds;

  (* Test cases for INV *)
  (* inv(1 + sqrt(2)) = 1 - sqrt(2) *)
  let x_inv = qr (r 1 1) (r 1 1) in
  R.eq (R.inv (ts x_inv)) (S.inv x_inv |> ts) |> R.adds;
  R.eq (ts (S.inv x_inv)) (ts (qr (r 1 1) (r (-1) 1))) |> R.adds;

  (* Test cases for DIV *)
  (* (8 + 5*sqrt(2)) / (2 + 3*sqrt(2)) = 1 + sqrt(2) *)
  let x_div = qr (r 8 1) (r 5 1) in
  let y_div = qr (r 2 1) (r 3 1) in
  R.eq (R.div (ts x_div) (ts y_div)) (S.div x_div y_div |> ts) |> R.adds;
  R.eq (ts (S.div x_div y_div)) (ts (qr (r 1 1) (r 1 1))) |> R.adds;

  let one_q = qr (r 1 1) (r 0 1) in
  R.eq (ts one_q) (r_ts (r 1 1)) |> R.adds;

  (* Test cases for pow *)
  (* (1 + sqrt(2))^2 = 1 + 2*sqrt(2) + 2 = 3 + 2*sqrt(2) *)
  let x_pow = qr (r 1 1) (r 1 1) in
  let y_exp = 2 in
  R.eq (R.pow (ts x_pow) (string_of_int y_exp)) (S.pow x_pow y_exp |> ts)
  |> R.adds;
  R.eq (ts (S.pow x_pow y_exp)) (ts (qr (r 3 1) (r 2 1))) |> R.adds;

  (* (sqrt(2))^-2 = 1/2 *)
  let x_pow_2 = sqrt2 in
  let y_exp_2 = -2 in
  R.eq (R.pow (ts x_pow_2) (string_of_int y_exp_2)) (S.pow x_pow_2 y_exp_2 |> ts)
  |> R.adds;
  R.eq (ts (S.pow x_pow_2 y_exp_2)) (ts (qr (r 1 2) (r 0 1))) |> R.adds

let () =
  test ();
  print_string (R.get_full_latex_document ())
