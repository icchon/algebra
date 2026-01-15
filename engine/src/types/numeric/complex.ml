module BASE = struct
  module S = struct
    include Rational

    type leaf = Rational.t

    let flatten (x : t) : leaf list = [ x ]
    let compare = Rational.compare
  end

  module CD =
    Cayley_dickson.Make
      (S)
      (struct
        include S

        let scale x s = S.mul x s
      end)

  include CD

  let compare (re1, im1) (re2, im2) =
    let c = S.compare re1 re2 in
    if c <> 0 then c else S.compare im1 im2

  include
    Cayley_dickson.InvExtend
      (S)
      (struct
        include S

        let scale = S.mul
        let norm_sq = S.norm_sq
      end)

  let v x = x
  let derive _ = zero
  (* let is_negative _ = false *)
  (* let is_one (re, im) = S.is_one re && S.is_zero im *)
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
