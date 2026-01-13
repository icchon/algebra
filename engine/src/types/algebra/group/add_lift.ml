module type SMIN = sig
  type t

  val zero : t
  val add : t -> t -> t
  val neg : t -> t
  val equal : t -> t -> bool
end

module Extend (S : SMIN) (V : Traversable.S) = struct
  let zero = V.pure S.zero
  let add v1 v2 = V.lift2 S.add v1 v2
  let neg v = V.map S.neg v
  let equal v1 v2 = V.lift2 S.equal v1 v2 |> V.fold_left ( && ) true
end
