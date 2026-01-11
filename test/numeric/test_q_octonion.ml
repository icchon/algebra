open Types
module R = Latex_reporter
module S = Q_octonion

(* Helper to create an octonion from a list of integer coeffs *)
let o (coeffs_int : int list) =
  let arr = Array.make S.size Rational.zero in
  List.iteri (fun i c -> arr.(i) <- Rational.of_int c) coeffs_int;
  arr

let i = o [ 0; 1; 0; 0; 0; 0; 0; 0 ]
let j = o [ 0; 0; 1; 0; 0; 0; 0; 0 ]
let k = o [ 0; 0; 0; 1; 0; 0; 0; 0 ]
let l = o [ 0; 0; 0; 0; 1; 0; 0; 0 ]
let il = o [ 0; 0; 0; 0; 0; 1; 0; 0 ]
let jl = o [ 0; 0; 0; 0; 0; 0; 1; 0 ]
let kl = o [ 0; 0; 0; 0; 0; 0; 0; 1 ]
let one = S.one
let neg_one = S.neg S.one

let test () =
  let ts = S.to_string_latex in

  R.section "Octonions over Q";

  R.subsection "Multiplication";
  R.eq (R.mul (ts i) (ts i)) (ts neg_one) |> R.adds;
  R.eq (R.mul (ts j) (ts j)) (ts neg_one) |> R.adds;
  R.eq (R.mul (ts k) (ts k)) (ts neg_one) |> R.adds;
  R.eq (R.mul (ts l) (ts l)) (ts neg_one) |> R.adds;
  R.eq (R.mul (ts il) (ts il)) (ts neg_one) |> R.adds;
  R.eq (R.mul (ts jl) (ts jl)) (ts neg_one) |> R.adds;
  R.eq (R.mul (ts kl) (ts kl)) (ts neg_one) |> R.adds;

  R.eq (R.mul (ts i) (ts j)) (ts k) |> R.adds;
  R.eq (R.mul (ts j) (ts i)) (ts (S.neg k)) |> R.adds;

  R.eq (R.mul (ts i) (ts l)) (ts il) |> R.adds;
  R.eq (R.mul (ts l) (ts i)) (ts (S.neg il)) |> R.adds;

  R.subsection "Associativity Failure";
  let left_assoc = S.mul (S.mul i j) l in
  let right_assoc = S.mul i (S.mul j l) in
  R.eq (R.mul (R.mul (ts i) (ts j)) (ts l)) (ts left_assoc) |> R.adds;
  R.eq (R.mul (ts i) (R.mul (ts j) (ts l))) (ts right_assoc) |> R.adds;
  R.neq (ts left_assoc) (ts right_assoc) |> R.adds;

  R.subsection "Inverse";
  let x = o [ 1; 2; 3; 4; 5; 6; 7; 8 ] in
  let inv_x = S.inv x in
  R.eq (R.mul (ts x) (ts inv_x)) (ts one) |> R.adds;

  ()

let () =
  test ();
  print_string (R.get_full_latex_document ())
