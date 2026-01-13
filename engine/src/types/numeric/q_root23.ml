module Q23Basis = struct
  type coeff = Rational.t
  type basis = Unit | Sqrt2 | Sqrt3 | Sqrt6

  let all = [| Unit; Sqrt2; Sqrt3; Sqrt6 |]
  let size = Array.length all
  let to_int = function Unit -> 0 | Sqrt2 -> 1 | Sqrt3 -> 2 | Sqrt6 -> 3

  let multiply b1 b2 =
    let r n = Rational.of_int n in
    match (b1, b2) with
    | Unit, x | x, Unit -> (r 1, x)
    | Sqrt2, Sqrt2 -> (r 2, Unit)
    | Sqrt3, Sqrt3 -> (r 3, Unit)
    | Sqrt6, Sqrt6 -> (r 6, Unit)
    | Sqrt2, Sqrt3 | Sqrt3, Sqrt2 -> (r 1, Sqrt6)
    | Sqrt2, Sqrt6 | Sqrt6, Sqrt2 -> (r 2, Sqrt3)
    | Sqrt3, Sqrt6 | Sqrt6, Sqrt3 -> (r 3, Sqrt2)
end

module EF = Extension_field.Make (Rational) (Q23Basis)
include EF
include Q23Basis

let basis_to_string b =
  match b with Unit -> "" | Sqrt2 -> "√2" | Sqrt3 -> "√3" | Sqrt6 -> "√6"

let basis_to_string_latex b =
  match b with
  | Unit -> ""
  | Sqrt2 -> "\\sqrt{2}"
  | Sqrt3 -> "\\sqrt{3}"
  | Sqrt6 -> "\\sqrt{6}"

let to_string_latex (alpha : t) =
  let parts =
    List.filter_map
      (fun (i, b) ->
        let coeff = alpha.(i) in

        if Rational.is_zero coeff then None
        else
          let basis_str = basis_to_string_latex b in

          let part_str =
            if b = Unit then Some (Rational.to_string_latex coeff)
            else if Rational.is_one coeff then Some basis_str
            else if Rational.is_one (Rational.neg coeff) then
              Some ("-" ^ basis_str)
            else Some (Rational.to_string_latex coeff ^ " " ^ basis_str)
          in

          part_str)
      (List.mapi (fun i b -> (i, b)) (Array.to_list all))
  in

  if parts = [] then "0"
  else
    let s = String.concat " + " parts in

    Str.global_replace (Str.regexp " \\+ -") " - " s
