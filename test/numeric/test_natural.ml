open Types
module R = Latex_reporter
module S = Natural

let test () =
  let ts = S.to_string_latex in
  let x, y = (3, 0) in
  R.eq (R.add (ts x) (ts y)) (S.add x y |> ts) |> R.adds;

  R.eq (R.add (ts x) (ts y)) (S.add x y |> ts) |> R.adds;
  let x = 7 in
  R.eq (R.conj (ts x)) (S.conj x |> ts) |> R.adds;
  let x = -7 in
  R.eq (R.conj (ts x)) (S.conj x |> ts) |> R.adds;

  let x, y = (3, 5) in
  R.eq (R.mul (ts x) (ts y)) (S.mul x y |> ts) |> R.adds;
  let x, y = (3, 0) in
  R.eq (R.mul (ts x) (ts y)) (S.mul x y |> ts) |> R.adds;

  let x, y = (5, 0) in
  R.eq (R.pow (ts x) (ts y)) (S.pow x y |> ts) |> R.adds;
  let x, y = (2, 10) in
  R.eq (R.pow (ts x) (ts y)) (S.pow x y |> ts) |> R.adds

let () =
  test ();
  print_string (R.get_full_latex_document ())
