module type ALGEBRAIC_RULE = sig
  type coeff
  type basis

  val all : basis array
  val size : int
  val to_int : basis -> int
  val multiply : basis -> basis -> coeff * basis
end

module Make (F : Field.S) (Rule : ALGEBRAIC_RULE with type coeff = F.t) = struct
  module MatrixEngine =
    Square.ExtendField
      (F)
      (struct
        let n = Rule.size
      end)

  type t = F.t array

  let basis_matrices =
    Array.map
      (fun bk ->
        let mat = Array.make_matrix Rule.size Rule.size F.zero in
        Array.iter
          (fun bj ->
            let coeff, b_res = Rule.multiply bk bj in
            mat.(Rule.to_int b_res).(Rule.to_int bj) <- coeff)
          Rule.all;
        mat)
      Rule.all

  let to_matrix (alpha : t) =
    let m = Array.make_matrix Rule.size Rule.size F.zero in
    Array.iteri
      (fun k coeff ->
        if not (F.is_zero coeff) then
          let b_mat = basis_matrices.(k) in
          for i = 0 to Rule.size - 1 do
            for j = 0 to Rule.size - 1 do
              let term = F.mul coeff b_mat.(i).(j) in
              m.(i).(j) <- F.add m.(i).(j) term
            done
          done)
      alpha;
    m

  let inv (alpha : t) =
    let m = to_matrix alpha in
    match MatrixEngine.inverse m with
    | Some inv_matrix -> Array.init Rule.size (fun i -> inv_matrix.(i).(0))
    | None -> failwith "division by zero"

  let add a b = Array.init Rule.size (fun i -> F.add a.(i) b.(i))
  let neg x = Array.map F.neg x
  let zero = Array.init Rule.size (fun _ -> F.zero)
  let one = Array.init Rule.size (function 0 -> F.one | _ -> F.zero)
  let equal a b = Array.for_all2 F.equal a b

  let mul a b =
    let m_a = to_matrix a in
    let b_vec = Array.init Rule.size (fun i -> [| b.(i) |]) in
    let res_mat = MatrixEngine.matmul m_a b_vec in
    Array.init Rule.size (fun i -> res_mat.(i).(0))

  let conj x = x

  let to_string x =
    let elems = Array.map F.to_string x |> Array.to_list in
    "[" ^ String.concat "; " elems ^ "]"

  let to_string_latex x =
    let elems = Array.map F.to_string_latex x |> Array.to_list in
    "\\left[ " ^ String.concat "; " elems ^ " \\right]"

  let to_string_latex_level _ x = to_string_latex x

  include Add_monoid.Extend (struct
    type nonrec t = t

    let add, zero, equal = (add, zero, equal)
  end)

  include Add_group.Extend (struct
    type nonrec t = t

    let add, neg = (add, neg)
  end)

  include Mul_monoid.Extend (struct
    type nonrec t = t

    let one, mul, equal = (one, mul, equal)
  end)

  include Mul_group.Extend (struct
    type nonrec t = t

    let mul, inv = (mul, inv)
  end)

  include Division_ring.Extend (struct
    type nonrec t = t

    let mul, inv, pow = (mul, inv, pow)
  end)
end
