module OctonionBasis = struct
  type coeff = Rational.t
  type basis = Unit | I | J | K | L | IL | JL | KL

  let all = [| Unit; I; J; K; L; IL; JL; KL |]
  let size = Array.length all

  let to_int x =
    match x with
    | Unit -> 0
    | I -> 1
    | J -> 2
    | K -> 3
    | L -> 4
    | IL -> 5
    | JL -> 6
    | KL -> 7

  let multiply b1 b2 =
    let one = Rational.one in
    let minus_one = Rational.neg Rational.one in
    match (b1, b2) with
    | Unit, x | x, Unit -> (one, x)
    | I, I | J, J | K, K | L, L | IL, IL | JL, JL | KL, KL -> (minus_one, Unit)
    | I, J -> (one, K)
    | I, K -> (minus_one, J)
    | I, L -> (one, IL)
    | I, IL -> (minus_one, L)
    | I, JL -> (minus_one, KL)
    | I, KL -> (one, JL)
    | J, K -> (one, I)
    | J, I -> (minus_one, K)
    | J, L -> (one, JL)
    | J, IL -> (one, KL)
    | J, JL -> (minus_one, L)
    | J, KL -> (minus_one, IL)
    | K, I -> (one, J)
    | K, J -> (minus_one, I)
    | K, L -> (one, KL)
    | K, IL -> (minus_one, JL)
    | K, JL -> (one, IL)
    | K, KL -> (minus_one, L)
    | L, I -> (minus_one, IL)
    | L, J -> (minus_one, JL)
    | L, K -> (minus_one, KL)
    | L, IL -> (one, I)
    | L, JL -> (one, J)
    | L, KL -> (one, K)
    | IL, I -> (one, L)
    | IL, J -> (minus_one, KL)
    | IL, K -> (one, JL)
    | IL, L -> (minus_one, I)
    | IL, JL -> (minus_one, K)
    | IL, KL -> (one, J)
    | JL, I -> (one, KL)
    | JL, J -> (one, L)
    | JL, K -> (minus_one, IL)
    | JL, L -> (minus_one, J)
    | JL, IL -> (one, K)
    | JL, KL -> (minus_one, I)
    | KL, I -> (minus_one, JL)
    | KL, J -> (one, IL)
    | KL, K -> (one, L)
    | KL, L -> (minus_one, K)
    | KL, IL -> (minus_one, J)
    | KL, JL -> (one, I)
end

module EF = Extension_field.Make (Rational) (OctonionBasis)
include EF
include OctonionBasis

let basis_to_string b =
  match b with
  | Unit -> ""
  | I -> "i"
  | J -> "j"
  | K -> "k"
  | L -> "l"
  | IL -> "il"
  | JL -> "jl"
  | KL -> "kl"

let basis_to_string_latex b =
  match b with
  | Unit -> ""
  | I -> "i"
  | J -> "j"
  | K -> "k"
  | L -> "l"
  | IL -> "\\mathrm{il}"
  | JL -> "\\mathrm{jl}"
  | KL -> "\\mathrm{kl}"

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
