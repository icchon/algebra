module BASE = struct
  include Fraction.Make (Integer)

  let comp ((n1, d1) : t) ((n2, d2) : t) =
    Integer.comp (Integer.mul n1 d2) (Integer.mul n2 d1)

  let v (x : t) : t = x
  let of_int (x : Integer.t) : t = (x, Integer.one)

  let cubic_root (n, d) =
    let find_root x =
      let target = abs x in
      let rec aux i =
        let cube = i * i * i in
        if cube = target then Some (if x < 0 then -i else i)
        else if cube > target then None
        else aux (i + 1)
      in
      aux 0
    in
    match (find_root n, find_root d) with
    | Some rn, Some rd -> Some (rn, rd)
    | _ -> None

  let square_root (n, d) =
    let find_root x =
      let rec aux i =
        let cube = i * i in
        if cube = x then Some i else if cube > x then None else aux (i + 1)
      in
      aux 0
    in
    match (find_root n, find_root d) with
    | Some rn, Some rd -> Some (rn, rd)
    | _ -> None
end

include BASE
module O = Ordered.Extend (BASE)
include O

include Division_ring.Extend (struct
  include BASE
end)

include Signed.Extend (struct
  include BASE
  include O
end)

include Product.Extend (BASE) (Container.Id)
include Formatter.ExtendNumericLatex (BASE)
