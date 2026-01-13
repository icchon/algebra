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
  let parse_string s = 
    match String.split_on_char '/' s with
    | [n] -> (int_of_string n, 1)
    | [n; d] -> 
    let d_int = int_of_string d in
    if d_int = 0 then failwith "Division by zero"
    else (int_of_string n, d_int)
    | _ -> failwith ("invalid rational format: " ^ s)

  let of_yojson json = 
    match json with 
    | `String s -> parse_string s 
    | `Int i -> (i, 1)
    | `List [`Int n; `Int d] -> (n, d)
    | _ -> failwith "Invalid rational format"
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
