module Extend
    (M : sig
      type t

      val conj : t -> t
      val mul : t -> t -> t
      val zero : t
      val add : t -> t -> t
    end)
    (V : Traversable.S) =
struct
  let dot x y =
    V.fold_left
      (fun acc a -> M.add acc a)
      M.zero
      (V.lift2 (fun a b -> M.mul a (M.conj b)) x y)

  let norm_sq x = dot x x
end
