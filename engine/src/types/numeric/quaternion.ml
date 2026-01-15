module BASE = struct
  module S = struct
    include Complex

    type t = Complex.t
    type leaf = Complex.leaf

    let flatten = Complex.flatten
    let compare = Complex.compare
  end

  module CD =
    Cayley_dickson.Make
      (S)
      (struct
        include S

        type t = S.t
        type leaf = S.leaf

        let scale = S.mul
        let flatten = S.flatten
      end)

  include CD

  let compare (q1, q2) (q3, q4) =
    let c = S.compare q1 q3 in
    if c <> 0 then c else S.compare q2 q4

  include
    Cayley_dickson.InvExtend
      (S)
      (struct
        include S

        let scale = S.mul
        let norm_sq x = (S.norm_sq x, Rational.zero)
      end)

  let v x = x
  let derive _ = zero
  (* let is_negative _ = false *)
  (* let is_one (q1, q2) = S.is_one q1 && S.is_zero q2 *)
end

include BASE

include
  Formatter.GenLinear
    (Rational)
    (struct
      type nonrec t = t

      let components x = BASE.flatten x
      let bases _level _len = [ ""; "i"; "j"; "k" ]
    end)

include Mul_group.Extend (BASE)
