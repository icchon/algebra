open Printf

let is_atomic_latex s =
  (* 単一の数字、変数、または既に \frac や \left(...\right) で囲まれているものを原子とみなす *)
  s = "" || true
(* let s_trimmed = String.trim s in
  s_trimmed = ""
  || Str.string_match (Str.regexp "^[0-9.-]+$") s_trimmed 0
  (* 数値 *)
  || Str.string_match (Str.regexp "^[a-zA-Z_][a-zA-Z0-9_]*\\^{.*}$") s_trimmed 0
  (* 単一変数の冪乗 *)
  || Str.string_match (Str.regexp "^e_{.*}$") s_trimmed 0 (* e_n の形式 *)
  || Str.string_match (Str.regexp "^[ijkl]$") s_trimmed 0
  (* 虚数単位 i, j, k *)
  || Str.string_match (Str.regexp "^\\\\\\\\frac{.*}{.*}$") s_trimmed 0
  || Str.string_match (Str.regexp "^\\\\left(.*\\\\right)$") s_trimmed 0
  || Str.string_match (Str.regexp "^\\\\det\\\\left(.*\\\\right)$") s_trimmed 0
  || Str.string_match
       (Str.regexp "^\\\\begin{pmatrix}.*\\\\end{pmatrix}$")
       s_trimmed 0 *)

let paren_if_needed s =
  if is_atomic_latex s then s else sprintf "\\left( %s \\right)" s

let paren s = sprintf "\\left( %s \\right)" s
let add s1 s2 = sprintf "%s + %s" (paren_if_needed s1) (paren_if_needed s2)
let sub s1 s2 = sprintf "%s - %s" (paren_if_needed s1) (paren_if_needed s2)
let mul s1 s2 = sprintf "%s \\cdot %s" (paren_if_needed s1) (paren_if_needed s2)
let coeff s1 s2 = sprintf "%s%s" (paren_if_needed s1) (paren_if_needed s2)
let div s1 s2 = sprintf "\\frac{%s}{%s}" s1 s2
let pow base exp = sprintf "%s^{%s}" (paren_if_needed base) exp

let dot s1 s2 =
  sprintf "\\langle{%s},{%s}\\rangle" (paren_if_needed s1) (paren_if_needed s2)
(* dotを追加 *)

let vector elements =
  sprintf "\\begin{pmatrix}%s\\end{pmatrix}" (String.concat "\\\\" elements)

let matrix rows =
  sprintf "\\begin{pmatrix}%s\\end{pmatrix}" (String.concat "\\\\" rows)
