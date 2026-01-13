module Make
    (S : Ring.S)
    (M : sig
      include Ring.S

      type leaf

      val flatten : t -> leaf list
      val scale : t -> S.t -> t
    end) =
struct
  type e = M.t
  type t = e * e
  type s = S.t
  type leaf = M.leaf

  let one = (M.one, M.zero)
  let flatten (a, b) = M.flatten a @ M.flatten b

  let mul (a, b) (c, d) =
    let t1 = M.sub (M.mul a c) (M.mul (M.conj d) b) in
    let t2 = M.add (M.mul d a) (M.mul b (M.conj c)) in
    (t1, t2)

  let conj (a, b) = (M.conj a, M.neg b)

  include Add_lift.Extend (M) (Container.Tup2)

  include Add_monoid.Extend (struct
    type nonrec t = t

    let zero, add, equal = (zero, add, equal)
  end)

  include Add_group.Extend (struct
    type nonrec t = t

    let add, neg = (add, neg)
  end)

  include Mul_monoid.Extend (struct
    type nonrec t = t

    let one, mul, equal = (one, mul, equal)
  end)

  include
    Product.Extend
      (struct
        type nonrec t = S.t

        let zero, add, mul, conj = (S.zero, S.add, S.mul, S.conj)
      end)
      (Container.Tup2)

  include
    Module.Extend
      (struct
        type e = M.t
        type s = S.t

        let scale = M.scale
      end)
      (Container.Tup2)
end

module InvExtend
    (S : Division_ring.S)
    (M : sig
      include Division_ring.S

      type leaf

      val flatten : t -> leaf list
      val norm_sq : t -> S.t
      val scale : t -> S.t -> t
    end) =
struct
  module C = Make (S) (M)

  let div_scale x s = C.mul_scale x (S.inv s)

  let inv x =
    let a, b = x in
    let n_sq = S.add (M.norm_sq a) (M.norm_sq b) in
    div_scale x n_sq
end
