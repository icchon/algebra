open Types
module R = Latex_reporter
module S = Complex

(* Helper to create a rational number *)
let r n d = (n, d)

(* Helper to create a complex number *)
let c re im = (re, im)

let test () =
  let ts = S.to_string_latex in
  let r_ts = Rational.to_string_latex in

  (* ADD *)
  let x, y = (c (r 1 2) (r 1 3), c (r 1 4) (r 1 5)) in
  R.eq (R.add (ts x) (ts y)) (S.add x y |> ts) |> R.adds;
  let x, y = (c (r 1 1) (r 2 1), c (r (-1) 1) (r (-2) 1)) in
  R.eq (R.add (ts x) (ts y)) (S.add x y |> ts) |> R.adds;

  (* SUB *)
  let x, y = (c (r 1 2) (r 1 3), c (r 1 4) (r 1 5)) in
  R.eq (R.sub (ts x) (ts y)) (S.sub x y |> ts) |> R.adds;
  let x, y = (c (r 1 1) (r 2 1), c (r 1 1) (r 2 1)) in
  R.eq (R.sub (ts x) (ts y)) (S.sub x y |> ts) |> R.adds;

  (* MUL *)
  (* (1+2i)(3+4i) = 3 + 4i + 6i + 8i^2 = 3 + 10i - 8 = -5 + 10i *)
  let x, y = (c (r 1 1) (r 2 1), c (r 3 1) (r 4 1)) in
  R.eq (R.mul (ts x) (ts y)) (ts (c (r (-5) 1) (r 10 1))) |> R.adds;

  (* i*i = -1 *)
  let i = c (r 0 1) (r 1 1) in
  R.eq (R.mul (ts i) (ts i)) (ts (c (r (-1) 1) (r 0 1))) |> R.adds;

  (* DIV *)
  (* ( -5 + 10i ) / ( 1 + 2i ) = 3 + 4i *)
  let x = c (r (-5) 1) (r 10 1) in
  let y = c (r 1 1) (r 2 1) in
  R.eq (R.div (ts x) (ts y)) (ts (c (r 3 1) (r 4 1))) |> R.adds;

  (* INV *)
  (* inv(3+4i) = (3-4i)/(9+16) = 3/25 - 4/25i *)
  let x = c (r 3 1) (r 4 1) in
  R.eq (R.inv (ts x)) (ts (c (r 3 25) (r (-4) 25))) |> R.adds;

  (* NEG *)
  let x = c (r 1 2) (r 3 4) in
  R.eq (R.neg (ts x)) (ts (c (r (-1) 2) (r (-3) 4))) |> R.adds;

  (* CONJ *)
  let x = c (r 1 2) (r 3 4) in
  R.eq (R.conj (ts x)) (ts (c (r 1 2) (r (-3) 4))) |> R.adds;

  (* NORM_SQ *)
  (* |3+4i|^2 = 9+16=25 *)
  let x = c (r 3 1) (r 4 1) in
  R.eq (R.norm_sq (ts x)) (r 25 1 |> r_ts) |> R.adds;

  (* POW *)
  (* (1+i)^2 = 1 + 2i - 1 = 2i *)
  let x = c (r 1 1) (r 1 1) in
  let y = 2 in
  R.eq (R.pow (ts x) (string_of_int y)) (ts (c (r 0 1) (r 2 1))) |> R.adds;

  (* (1+i)^-1 = (1-i)/2 = 1/2 - 1/2i *)
  let y = -1 in
  R.eq (R.pow (ts x) (string_of_int y)) (ts (c (r 1 2) (r (-1) 2))) |> R.adds

let () =
  test ();
  print_string (R.get_full_latex_document ())
