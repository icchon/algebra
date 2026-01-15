module Make (S : Ring.S) (M : Dim.S) (N : Dim.S) = struct
  module Core = Container.Rectangular (M) (N)
  include Core

  type t = S.t Core.t

  module AL = Add_lift.Extend (S) (Core)
  include AL

  include Add_monoid.Extend (struct
    type nonrec t = t

    let zero, add, equal = (AL.zero, AL.add, AL.equal)
  end)

  include Add_group.Extend (struct
    type nonrec t = t

    include AL
  end)

  include
    Module.Extend
      (struct
        include S

        type s = S.t
        type e = S.t

        let scale = S.mul
      end)
      (Core)

  let transpose m =
    let r = Array.length m in
    if r = 0 then [||]
    else
      let c = Array.length m.(0) in
      Array.init c (fun j -> Array.init r (fun i -> m.(i).(j)))

  let conj m = Core.map S.conj m |> transpose

  let matmul m1 m2 =
    let r1 = Array.length m1 in
    let c1 = Array.length m1.(0) in
    let r2 = Array.length m2 in
    let c2 = Array.length m2.(0) in
    if c1 <> r2 then failwith "Dimension mismatch";
    let m2_t = transpose m2 in
    Array.init r1 (fun i ->
        Array.init c2 (fun j ->
            let products =
              Array.init c1 (fun k -> S.mul m1.(i).(k) m2_t.(j).(k))
            in
            if c1 = 0 then failwith "Cannot multiply empty matrices"
            else
              Array.fold_left S.add products.(0) (Array.sub products 1 (c1 - 1))))

  let identity n =
    Array.init n (fun i ->
        Array.init n (fun j -> if i = j then S.one else S.zero))

  include Formatter.GenMatrix (struct
    include S
    include Formatter.ExtendNumericLatex (S)

    let is_negative _ = false
  end)
end

module ExtendField (S : Field.S) (M : Dim.S) (N : Dim.S) = struct
  module Core = Container.Rectangular (M) (N)

  include
    Vector_space.Extend
      (S)
      (struct
        type e = S.t

        let scale = S.mul
      end)
      (Core)
end
