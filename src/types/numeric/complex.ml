module BASE = struct
  module S = struct
    include Rational

    type leaf = Rational.t

    let flatten (x : t) : leaf list = [ x ]
  end

  module CD =
    Cayley_dickson.Make
      (S)
      (struct
        include S

        let scale x s = S.mul x s
      end)

  include CD

  include
    Cayley_dickson.InvExtend
      (S)
      (struct
        include S

        let scale = S.mul
        let norm_sq = S.norm_sq
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
      let bases _level _len = [ ""; "i" ]
    end)

include Mul_group.Extend (BASE)
