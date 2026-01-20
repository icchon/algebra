module type Config = sig
  type t

  val zero : t
  val is_negative : t -> bool
  val is_one : t -> bool
  val neg : t -> t
  val to_string : t -> string
  val to_string_latex : t -> string
  val to_string_latex_level : int -> t -> string
end

(* 構造側が「中身」と「基底」をどう取り出すかを定義する *)
module type STRUCTURE = sig
  type t
  type coeff

  val components : t -> coeff list
  val bases : int -> int -> string list (* level -> length -> bases *)
end

module GenLinear (C : Config) (S : STRUCTURE with type coeff := C.t) = struct
  let build_linear ~level ~is_latex (x : S.t) =
    let cs = S.components x in
    let bs = S.bases level (List.length cs) in
    let to_s =
      if is_latex then C.to_string_latex_level level else C.to_string
    in

    let rec aux cs bs is_first =
      match (cs, bs) with
      | [], _ | _, [] -> []
      | c :: c_rest, b :: b_rest ->
          if c = C.zero then aux c_rest b_rest is_first
          else
            let is_neg = C.is_negative c in
            let abs_c = if is_neg then C.neg c else c in
            let c_str = to_s abs_c in

            let term =
              if b <> "" && C.is_one abs_c then b
              else
                let needs_paren = b <> "" && String.contains c_str ' ' in
                let c_part =
                  if needs_paren then
                    if is_latex then "\\left(" ^ c_str ^ "\\right)"
                    else "(" ^ c_str ^ ")"
                  else c_str
                in
                c_part ^ b
            in

            let op =
              if is_first then if is_neg then "-" else ""
              else if is_neg then " - "
              else if String.length term > 0 && term.[0] = '-' then " "
              else " + "
            in
            (op ^ term) :: aux c_rest b_rest false
    in
    match aux cs bs true with
    | [] -> to_s C.zero
    | parts -> String.concat "" parts

  (* 完璧に t -> string のシグネチャを守る *)
  let to_string (x : S.t) : string =
    let s = build_linear ~level:0 ~is_latex:false x in
    if String.contains s ' ' then "(" ^ s ^ ")" else s

  let to_string_latex_level (level : int) (x : S.t) : string =
    build_linear ~level ~is_latex:true x

  let to_string_latex (x : S.t) : string = to_string_latex_level 0 x
end

module GenMatrix (C : Config) = struct
  let to_string m =
    let rows =
      Array.map
        (fun row ->
          let s =
            Array.map C.to_string row |> Array.to_list |> String.concat ", "
          in
          "  [ " ^ s ^ " ]")
        m
    in
    "[\n" ^ String.concat "\n" (Array.to_list rows) ^ "\n]"

  let to_string_latex m =
    let rows_str =
      Array.map
        (fun row ->
          Array.map C.to_string_latex row
          |> Array.to_list |> String.concat " & ")
        m
      |> Array.to_list
    in
    Latex.matrix rows_str

  let to_string_latex_level _ m = to_string_latex m
end

(* ベクトル（Tuple）用のフォーマッタ *)
module GenVector (C : Config) = struct
  let to_string v =
    let s = List.map C.to_string v |> String.concat ", " in
    "(" ^ s ^ ")"

  let to_string_latex v =
    let s = List.map C.to_string_latex v |> String.concat ", " in
    "\\left( " ^ s ^ " \\right)"

  let to_string_latex_level _ v = to_string_latex v
end

module ExtendNumericLatex (M : sig
  type t

  val to_string_latex : t -> string
end) : sig
  type t = M.t

  val to_string_latex_level : int -> t -> string
end = struct
  type t = M.t

  let to_string_latex_level _ x = M.to_string_latex x
end

module Fraction (C : Config) = struct
  let build_string ~level ~is_latex (n, d) =
    (* 符号の判定に Config.is_negative を活用 *)
    let is_neg = C.is_negative n in
    let abs_n = if is_neg then C.neg n else n in

    (* 分母が 1 (is_one) なら分子のみを表示 *)
    let body =
      if C.is_one d then
        if is_latex then C.to_string_latex_level level abs_n
        else C.to_string abs_n
      else if is_latex then
        "\\frac{"
        ^ C.to_string_latex_level level abs_n
        ^ "}{"
        ^ C.to_string_latex_level level d
        ^ "}"
      else C.to_string abs_n ^ "/" ^ C.to_string d
    in

    (* 負なら外側に "-" を置く *)
    if is_neg then "-" ^ body else body

  let to_string x = build_string ~level:0 ~is_latex:false x
  let to_string_latex x = build_string ~level:0 ~is_latex:true x
  let to_string_latex_level level x = build_string ~level ~is_latex:true x
end
