module BASE = struct
  module Coeff = Rational
  module Poly = Polynomial.Make (Coeff)
  include Poly

  let min_poly = [| Coeff.of_int (-2); Coeff.zero; Coeff.one |]

  let mul a b =
    let prod = Poly.mul a b in
    Poly.rem prod min_poly

  let inv a =
    if Poly.equal a Poly.zero then failwith "Division by zero"
    else
      let g, s, _ = Poly.gcd_ext a min_poly in
      let lead = g.(0) in
      Poly.div_scale s lead

  module MM = Mul_monoid.Extend (struct
    type nonrec t = t

    let one, mul, equal = (Poly.one, mul, Poly.equal)
  end)

  include MM

  include Mul_group.Extend (struct
    type nonrec t = t

    let inv, mul = (inv, mul)
  end)

  include Division_ring.Extend (struct
    type nonrec t = t

    let mul, pow, inv = (mul, MM.pow, inv)
  end)

  let to_string p =
    let a = if Array.length p > 0 then p.(0) else Coeff.zero in
    let b = if Array.length p > 1 then p.(1) else Coeff.zero in
    Printf.sprintf "%s + (%s)sqrt(2)" (Coeff.to_string a) (Coeff.to_string b)

  let to_string_latex p =
    let a = if Array.length p > 0 then p.(0) else Coeff.zero in
    let b = if Array.length p > 1 then p.(1) else Coeff.zero in
    let a_str = Coeff.to_string_latex a in
    let b_str = Coeff.to_string_latex b in
    match (Coeff.is_zero a, Coeff.is_zero b) with
    | true, true -> "0"
    | false, true -> a_str
    | true, false ->
        if Coeff.is_one b then "\\sqrt{2}"
        else Printf.sprintf "%s \\sqrt{2}" b_str
    | false, false ->
        let op = if Coeff.is_negative b then "" else "+" in
        let b_part =
          if Coeff.is_one b then "\\sqrt{2}"
          else if Coeff.is_one (Coeff.neg b) then "-\\sqrt{2}"
          else Printf.sprintf "%s \\sqrt{2}" b_str
        in
        Printf.sprintf "%s %s %s" a_str op b_part

  let v x = x
  let from_array arr = normalize arr
end

include BASE
module Coeff = BASE.Coeff
module Poly = BASE.Poly
