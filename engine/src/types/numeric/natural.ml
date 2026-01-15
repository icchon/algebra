module BASE = struct
  type t = int

  let zero = 0
  let add = ( + )
  let one = 1
  let mul = ( * )
  let quo = ( / )
  let rem = ( mod )
  let comp = compare
  let compare = comp
  let equal x y = comp x y = 0
  (* let is_negative _ = false *)
  (* let is_one x = x = 1 *)
  let conj x = x
  let to_string x = string_of_int x
  let to_string_latex x = string_of_int x
  let v x = abs x
  let derive _ = zero
end

include BASE
include Add_monoid.Extend (BASE)
include Mul_monoid.Extend (BASE)
include Ordered.Extend (BASE)
include Formatter.ExtendNumericLatex (BASE)
include Product.Extend (BASE) (Container.Id)
