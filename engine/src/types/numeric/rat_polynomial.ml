module BASE = struct
  include Polynomial.Make (Rational)

  let v x = x
  let from_array x = normalize x

  let compose p q =
    let module V = struct
      include Polynomial.Make (Rational)

      type s = Rational.t
    end in
    eval (module V) p q
end

include BASE
