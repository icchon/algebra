module type Evaluatable = sig
  include Ring.S

  type s

  include Module.S with type s := s and type t := t
end

module Make (Coeff : sig
  include Field.S

  val to_string_latex_level : int -> t -> string
  val is_negative : t -> bool
  val is_one : t -> bool
end) =
struct
  type coeff = Coeff.t
  type t = coeff array

  let normalize p =
    let rec find_last i =
      if i < 0 then -1
      else if Coeff.equal p.(i) Coeff.zero then find_last (i - 1)
      else i
    in
    let last = find_last (Array.length p - 1) in
    if last = -1 then [||]
    else if last = Array.length p - 1 then p
    else Array.sub p 0 (last + 1)

  let degree p = Array.length p - 1
  let map_coeff f p = Array.map f p |> normalize

  let lift2_coeff f p1 p2 =
    let l1, l2 = (Array.length p1, Array.length p2) in
    Array.init (max l1 l2) (fun i ->
        let c1 = if i < l1 then p1.(i) else Coeff.zero in
        let c2 = if i < l2 then p2.(i) else Coeff.zero in
        f c1 c2)
    |> normalize

  let pure c = if Coeff.equal c Coeff.zero then [||] else [| c |]
  let zero = [||]
  let one = pure Coeff.one
  let add p1 p2 = lift2_coeff Coeff.add p1 p2
  let neg p = Array.map Coeff.neg p

  let equal p1 p2 =
    let l1, l2 = (Array.length p1, Array.length p2) in
    if l1 <> l2 then false
    else
      let rec aux i =
        if i < 0 then true
        else if not (Coeff.equal p1.(i) p2.(i)) then false
        else aux (i - 1)
      in
      aux (l1 - 1)

  include Add_monoid.Extend (struct
    type nonrec t = t

    let zero, add, equal = (zero, add, equal)
  end)

  include Add_group.Extend (struct
    type nonrec t = t

    let add, neg = (add, neg)
  end)

  let conj p = p

  let mul p1 p2 =
    let l1, l2 = (Array.length p1, Array.length p2) in
    if l1 = 0 || l2 = 0 then zero
    else
      let res = Array.make (l1 + l2 - 1) Coeff.zero in
      Array.iteri
        (fun i c1 ->
          Array.iteri
            (fun j c2 ->
              let idx = i + j in
              res.(idx) <- Coeff.add res.(idx) (Coeff.mul c1 c2))
            p2)
        p1;
      res |> normalize

  let div_rem_impl a b =
    if Array.length b = 0 then failwith "Division by zero polynomial"
    else
      let rec aux q r =
        let deg_r, deg_b = (degree r, degree b) in
        if deg_r < deg_b then (q, r)
        else
          let lead_r, lead_b = (r.(deg_r), b.(deg_b)) in
          let q_curr_coeff = Coeff.div lead_r lead_b in
          let shift = deg_r - deg_b in
          let q_step = Array.make (shift + 1) Coeff.zero in
          q_step.(shift) <- q_curr_coeff;
          aux (add q q_step) (add r (neg (mul q_step b)))
      in
      aux zero a

  let quo a b = fst (div_rem_impl a b)
  let rem a b = snd (div_rem_impl a b)

  include Euclidean.Extend (struct
    type nonrec t = t

    let zero, one = (zero, one)
    let sub, mul = (sub, mul)
    let quo, rem = (quo, rem)
  end)

  module MM = Mul_monoid.Extend (struct
    type nonrec t = t

    let one, mul, equal = (one, mul, equal)
  end)

  include MM

  module CoeffAct = struct
    type s = Coeff.t
    type e = Coeff.t

    let scale e s = Coeff.mul e s
  end

  include Module.Extend (CoeffAct) (Container.Arr)
  include Vector_space.Extend (Coeff) (CoeffAct) (Container.Arr)

  let normalize_leading_coeff p =
    if Array.length p = 0 then p else div_scale p p.(Array.length p - 1)

  let gcd a b = normalize_leading_coeff (gcd a b)

  let get_var_name = function
    | 0 -> "x"
    | 1 -> "y"
    | 2 -> "z"
    | 3 -> "w"
    | n -> "x_{" ^ string_of_int (n + 1) ^ "}"

  include
    Formatter.GenLinear
      (Coeff)
      (struct
        type nonrec t = t

        let components p = Array.to_list p

        let bases level len =
          let var = get_var_name level in
          List.init len (function
            | 0 -> ""
            | 1 -> var
            | i -> var ^ "^{" ^ string_of_int i ^ "}")
      end)

  let eval (type v) (module V : Evaluatable with type s = coeff and type t = v)
      p x =
    Array.fold_right
      (fun c acc -> V.add (V.mul_scale V.one c) (V.mul acc x))
      p V.zero
end
