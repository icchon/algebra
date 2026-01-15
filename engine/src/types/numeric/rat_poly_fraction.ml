module BASE = struct
  include Fraction.Make (Rat_polynomial)

  let v x = x
end

include BASE
