module Make (Coeff : sig
  include Euclidean.S
  include Formatter.Config with type t := t

  val abs : t -> t
end) =
struct
  type coeff = Coeff.t
  type t = coeff * coeff

  let normalize (n, d) =
    if Coeff.(equal d zero) then failwith "Division by zero"
    else
      let n, d =
        if Coeff.is_negative d then (Coeff.neg n, Coeff.neg d) else (n, d)
      in
      let g = Coeff.abs (Coeff.gcd n d) in
      (Coeff.quo n g, Coeff.quo d g)

  let v x = normalize x
  let zero = (Coeff.zero, Coeff.one)
  let one = (Coeff.one, Coeff.one)
  let neg (n, d) = (Coeff.neg n, d)
  let conj (n, d) = normalize (Coeff.conj n, Coeff.conj d)

  let add (n1, d1) (n2, d2) =
    normalize (Coeff.add (Coeff.mul n1 d2) (Coeff.mul n2 d1), Coeff.mul d1 d2)

  let mul (n1, d1) (n2, d2) = normalize (Coeff.mul n1 n2, Coeff.mul d1 d2)
  let inv (n, d) = normalize (d, n)
  let equal (n1, d1) (n2, d2) = Coeff.equal (Coeff.mul n1 d2) (Coeff.mul n2 d1)

  module AM = Add_monoid.Extend (struct
    type nonrec t = t

    let zero, add, equal = (zero, add, equal)
  end)

  include AM

  module AG = Add_group.Extend (struct
    type nonrec t = t

    let add, neg = (add, neg)
  end)

  include AG

  module MM = Mul_monoid.Extend (struct
    type nonrec t = t

    let one, mul, equal = (one, mul, equal)
  end)

  include MM

  module MG = Mul_group.Extend (struct
    type nonrec t = t

    let mul, inv = (mul, inv)
  end)

  include MG
  include Formatter.Fraction (Coeff)
end
