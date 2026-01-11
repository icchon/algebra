open Printf

let to_string ~zero ~is_neg ~to_string ~components ~bases =
  let rec build_parts i comps bs acc_is_empty =
    match (comps, bs) with
    | [], _ | _, [] -> []
    | c :: c_rest, b :: b_rest ->
        if c = zero then build_parts (i + 1) c_rest b_rest acc_is_empty
        else
          let is_c_neg = is_neg c in
          let c_str = to_string c in
          let c_abs_str =
            if is_c_neg then String.sub c_str 1 (String.length c_str - 1)
            else c_str
          in

          let term_val =
            if c_abs_str = "1" && i != 0 then b else c_abs_str ^ b
          in

          let formatted_term =
            if acc_is_empty then if is_c_neg then "-" ^ term_val else term_val
            else if is_c_neg then "-" ^ term_val
            else "+" ^ term_val
          in
          formatted_term :: build_parts (i + 1) c_rest b_rest false
  in
  match build_parts 0 components bases true with
  | [] -> to_string zero
  | [ single ] -> single
  | parts -> "(" ^ String.concat "" parts ^ ")"

let to_string_latex ~zero ~is_one_fn ~is_negative ~to_string_fn ~neg_fn
    ~components ~bases =
  let rec build_terms_with_signs i comps bs acc =
    match (comps, bs) with
    | [], _ | _, [] -> List.rev acc
    | c :: c_rest, b :: b_rest ->
        if c = zero then build_terms_with_signs (i + 1) c_rest b_rest acc
        else
          let c_is_negative = is_negative c in
          let abs_c = if c_is_negative then neg_fn c else c in

          let term_str =
            if b = "" then to_string_fn abs_c
            else if is_one_fn abs_c then b
            else sprintf "%s%s" (to_string_fn abs_c) b
          in
          build_terms_with_signs (i + 1) c_rest b_rest
            ((term_str, c_is_negative) :: acc)
  in
  (* ... 以下、符号結合ロジックは変更なし ... *)
  let terms_with_signs = build_terms_with_signs 0 components bases [] in
  match terms_with_signs with
  | [] -> to_string_fn zero
  | (head_term, head_is_negative) :: tail_terms ->
      let current_str =
        ref (if head_is_negative then sprintf "-%s" head_term else head_term)
      in
      List.iter
        (fun (term_str, term_is_negative) ->
          if term_is_negative then
            current_str := sprintf "%s - %s" !current_str term_str
          else current_str := sprintf "%s + %s" !current_str term_str)
        tail_terms;
      Latex.paren_if_needed !current_str
