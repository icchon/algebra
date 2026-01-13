module BASE = struct
  type t = int

  let zero = 0
  let add = ( + )
  let one = 1
  let mul = ( * )
  let quo = ( / )
  let rem = ( mod )
  let comp = compare
  let equal x y = comp x y = 0
  let conj x = x
  let to_string x = string_of_int x
  let to_string_latex x = string_of_int x
  let v x = abs x
end

include BASE
include Add_monoid.Extend (BASE)
include Mul_monoid.Extend (BASE)
include Ordered.Extend (BASE)
include Formatter.ExtendNumericLatex (BASE)
include Product.Extend (BASE) (Container.Id)
