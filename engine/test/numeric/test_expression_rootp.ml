open Types
module E = Expression.Expression
module R = Latex_reporter

let test () =
  let ts = E.to_string_latex in
  
  R.section "Expression Tower with RootP Base Field";

  (* 1. RootP Arithmetic in Expression *)
  let sqrt2 = Q_rootp.make_root 2 in
  let e_sqrt2 = E.RootP sqrt2 in
  let e_sqrt3 = E.RootP (Q_rootp.make_root 3) in
  
  let sum = E.add e_sqrt2 e_sqrt3 in
  let prod = E.mul sum sum in
  
  R.subsection "RootP Arithmetic";
  R.eq (R.add (ts e_sqrt2) (ts e_sqrt3)) (ts sum) |> R.adds;
  R.eq (R.pow (R.group (ts sum)) "2") (ts (E.eval prod)) |> R.adds;

  (* 2. Complex Arithmetic via RootP (-1) *)
  let i = Q_rootp.make_root (-1) in
  let e_i = E.RootP i in
  let complex_sum = E.add E.one e_i in (* 1 + i *)
  let complex_prod = E.mul complex_sum complex_sum in (* (1+i)^2 = 2i *)
  
  R.subsection "Complex Arithmetic via RootP";
  R.eq (R.pow (R.group (ts complex_sum)) "2") (ts (E.eval complex_prod)) |> R.adds;

  (* 3. Quaternion with RootP *)
  (* Let's create a quaternion with root coeffs *)
  (* Quaternion is (Complex.t * Complex.t) where Complex.t is (Q_rootp.t * Q_rootp.t) *)
  let q_val = ( (sqrt2, Q_rootp.zero), (Q_rootp.zero, Q_rootp.zero) ) in (* sqrt(2) + 0i + 0j + 0k *)
  let e_q = E.Quaternion q_val in
  
  R.subsection "Quaternion with RootP";
  R.eq "q" (ts e_q) |> R.adds;

  ()

let () =
  test ();
  print_string (R.get_full_latex_document ())
