open Printf

let paren s = sprintf "\\left( %s \\right)" s

let is_atomic_latex s =
  let s_trimmed = String.trim s in
  s_trimmed = ""
  (* 1. 数字のみ (-1.5, 42) *)
  || Str.string_match (Str.regexp "^-?[0-9.]+$") s_trimmed 0
  (* 2. 数字(任意) + アルファベット(1文字以上) (2i, 5ab, -xyz, sin) *)
  || Str.string_match (Str.regexp "^-?[0-9.]*[a-zA-Z]+$") s_trimmed 0
  (* 3. 数字(任意) + 基底ベクトル (2e_{1}, -e_{15}) *)
  || Str.string_match (Str.regexp "^-?[0-9.]*e_{.*}$") s_trimmed 0
  (* 4. 既存ルール（分数、カッコ済み、pmatrix、行列式） *)
  || Str.string_match (Str.regexp "^\\\\frac{[^{}]*}{[^{}]*}$") s_trimmed 0
  || Str.string_match (Str.regexp "^\\\\left(.*\\\\right)$") s_trimmed 0
  || Str.string_match
       (Str.regexp "^\\\\begin{pmatrix}\\(.\\|\n\\)*\\\\end{pmatrix}$")
       s_trimmed 0
  || Str.string_match (Str.regexp "^\\\\det\\\\left(.*\\\\right)$") s_trimmed 0

let paren_if_needed s =
  if is_atomic_latex s then s else sprintf "\\left( %s \\right)" s

let latex_outputs : string list ref = ref []
let adds s = latex_outputs := !latex_outputs @ [ s ]
(* latex_reporter.ml *)

(* 特殊文字をエスケープするか、数式を保護する *)
let section s = adds (sprintf "\\section*{%s}" s)
let subsection s = adds (sprintf "\\subsection*{%s}" s)
(* let section _ = print_endline ""
let subsection _ = print_endline "" *)
(* let adds s = add_latex_string s *)

let text s = sprintf "%s\\\\" s

(* let eq left_latex right_latex = sprintf "\\[ %s = %s \\]" left_latex right_latex *)
let eq left_latex right_latex =
  sprintf "\\begin{dmath*}\n %s = %s \n\\end{dmath*}" left_latex right_latex

let neq left_latex right_latex =
  sprintf "\\[ %s \\neq %s \\]" left_latex right_latex

let op left_latex op right_latex =
  sprintf "\\[ %s %s %s \\]" left_latex op right_latex

let add x y = sprintf "%s + %s" (paren_if_needed x) (paren_if_needed y)
let sub x y = sprintf "%s - %s" (paren_if_needed x) (paren_if_needed y)
let mul x y = sprintf "%s \\cdot %s" (paren_if_needed x) (paren_if_needed y)
let gcd x y = sprintf "\\gcd(%s, %s)" (paren_if_needed x) (paren_if_needed y)
let div x y = sprintf "%s \\div %s" (paren_if_needed x) (paren_if_needed y)
let pow base exp = sprintf "%s^{%s}" (paren base) exp
let transpose x = pow x "-1"
let star x = pow x "*"
let inv x = pow x "-1"

let dot x y =
  sprintf "\\langle{%s},{%s}\\rangle" (paren_if_needed x) (paren_if_needed y)

let norm_sq x = sprintf "\\| %s \\|^{2}" x
let abs x = sprintf "\\left| %s \\right|" (paren_if_needed x)
let conj x = sprintf "\\overline{%s}" (paren_if_needed x)

let add_scalar s x =
  sprintf "%s \\oplus %s" (paren_if_needed s) (paren_if_needed x)

let sub_scalar s x =
  sprintf "%s \\ominus %s" (paren_if_needed s) (paren_if_needed x)

let mul_scalar s x =
  sprintf "%s \\odot %s" (paren_if_needed s) (paren_if_needed x)

let div_scalar s x =
  sprintf "%s \\oslash %s" (paren_if_needed s) (paren_if_needed x)

let neg x = sprintf "-%s" (paren x)
let det x = sprintf "\\det\\left( %s \\right)" (paren_if_needed x)
(* latex_reporter.ml に以下を追加 *)

(* 既存のコードに追加 *)

let group s = paren s

(* 体の記号: \mathbb{Q} *)
let mathbb_q = "\\mathbb{Q}"

(* 平方根: \sqrt{x} *)
let sqrt s = sprintf "\\sqrt{%s}" s

(* 和の記号: \sum_{sub} body *)
let sum sub body = sprintf "\\sum_{%s} %s" sub body

(* 集合の表記: {a, b, c} *)
let set elements =
  sprintf "\\left\\{ %s \\right\\}" (String.concat ", " elements)

(* 所属: x \in X *)
let member x set = sprintf "%s \\in %s" x set

(* 体の記号: \mathbb{Q} など *)
let mathbb s = sprintf "\\mathbb{%s}" s

(* 累乗根: \sqrt[n]{x} *)
let root_n n x = sprintf "\\sqrt[%d]{%s}" n x
let root x = sprintf "\\sqrt{%s}" x

(* 複合演算の等式: 左辺 = 右辺 *)
let eq_raw left right = sprintf "\\[ %s = %s \\]" left right

let get_full_latex_document () =
  let header =
    "\\documentclass[12pt]{article}\n\
     \\usepackage[utf8]{inputenc}\n\
     \\usepackage{amsmath}\n\
     \\usepackage{amssymb}\n\
     \\usepackage{geometry}\n\
     \\usepackage{breqn}\n\
     \\geometry{a4paper, margin=1in}\n\
     \\begin{document}\n"
  in
  let footer = "\\end{document}\n" in
  header ^ String.concat "\n" !latex_outputs ^ footer
