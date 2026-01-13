open Types
module R = Latex_reporter
module S = Rational

let test () =
  let ts = S.to_string_latex in

  (* Test cases for add *)
  let x, y = ((1, 2), (1, 3)) in
  R.eq (R.add (ts x) (ts y)) (S.add x y |> ts) |> R.adds;
  let x, y = ((1, 2), (-1, 2)) in
  R.eq (R.add (ts x) (ts y)) (S.add x y |> ts) |> R.adds;

  (* Test cases for sub *)
  let x, y = ((1, 2), (1, 3)) in
  R.eq (R.sub (ts x) (ts y)) (S.sub x y |> ts) |> R.adds;
  let x, y = ((1, 2), (1, 2)) in
  R.eq (R.sub (ts x) (ts y)) (S.sub x y |> ts) |> R.adds;

  (* Test cases for mul *)
  let x, y = ((1, 2), (1, 3)) in
  R.eq (R.mul (ts x) (ts y)) (S.mul x y |> ts) |> R.adds;
  let x, y = ((2, 3), (3, 2)) in
  R.eq (R.mul (ts x) (ts y)) (S.mul x y |> ts) |> R.adds;

  (* Test cases for div *)
  let x, y = ((1, 2), (1, 3)) in
  R.eq (R.div (ts x) (ts y)) (S.div x y |> ts) |> R.adds;
  let x, y = ((2, 3), (2, 3)) in
  R.eq (R.div (ts x) (ts y)) (S.div x y |> ts) |> R.adds;

  (* Test cases for inv *)
  let x = (2, 3) in
  R.eq (R.inv (ts x)) (S.inv x |> ts) |> R.adds;
  let x = (-4, 5) in
  R.eq (R.inv (ts x)) (S.inv x |> ts) |> R.adds;

  (* Test cases for neg *)
  let x = (2, 3) in
  R.eq (R.neg (ts x)) (S.neg x |> ts) |> R.adds;
  let x = (-4, 5) in
  R.eq (R.neg (ts x)) (S.neg x |> ts) |> R.adds;

  (* Test cases for abs *)
  let x = (-2, 3) in
  R.eq (R.abs (ts x)) (S.abs x |> ts) |> R.adds;
  let x = (4, 5) in
  R.eq (R.abs (ts x)) (S.abs x |> ts) |> R.adds;

  (* Test cases for pow *)
  let x, y = ((2, 3), 3) in
  R.eq (R.pow (ts x) (string_of_int y)) (S.pow x y |> ts) |> R.adds;
  let x, y = ((1, 2), 0) in
  R.eq (R.pow (ts x) (string_of_int y)) (S.pow x y |> ts) |> R.adds;
  let x, y = ((1, 2), -2) in
  R.eq (R.pow (ts x) (string_of_int y)) (S.pow x y |> ts) |> R.adds;

  (* of_int *)
  let x = 5 in
  R.eq (ts (S.of_int x)) "5" |> R.adds;
  let x = -3 in
  R.eq (ts (S.of_int x)) "-3" |> R.adds

let () =
  test ();
  print_string (R.get_full_latex_document ())
