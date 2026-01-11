module type S = sig
  type 'a t

  val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
end
