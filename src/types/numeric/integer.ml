module BASE = struct
  type t = int

  let zero = 0
  let neg x = -x
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
  let v x = x

  let get_divisors n =
    let n = abs n in
    if n = 0 then []
    else if n = 1 then [ 1 ]
    else
      let rec aux i acc =
        if i * i > n then acc
        else if i * i = n then i :: acc
        else if n mod i = 0 then aux (i + 1) (i :: (n / i) :: acc)
        else aux (i + 1) acc
      in
      aux 1 [] |> List.sort Int.compare
end

include BASE
include Add_monoid.Extend (BASE)
include Mul_monoid.Extend (BASE)
module O = Ordered.Extend (BASE)
include O
include Formatter.ExtendNumericLatex (BASE)
include Product.Extend (BASE) (Container.Id)

include Signed.Extend (struct
  include BASE
  include O
end)

module AG = Add_group.Extend (BASE)
include AG

include Euclidean.Extend (struct
  include BASE
  include AG
end)
