module type S = sig
  type 'a t

  val pure : 'a -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val lift2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
end
