module BASE = struct
  module S = struct
    include Complex

    type t = Complex.t
    type leaf = Complex.leaf

    let flatten = Complex.flatten
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

  include
    Cayley_dickson.InvExtend
      (S)
      (struct
        include S

        let scale = S.mul
        let norm_sq x = (S.norm_sq x, Rational.zero)
      end)

  let v x = x
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
