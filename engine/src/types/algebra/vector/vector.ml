module Make (S : Field.S) (N : Dim.S) = struct
  module Core = Container.Vector (N)
  include Core

  type t = S.t Core.t

  include Add_lift.Extend (S) (Core)

  include
    Module.Extend
      (struct
        type s = S.t
        type e = S.t

        let scale = S.mul
      end)
      (Core)

  include
    Vector_space.Extend
      (S)
      (struct
        type e = S.t

        let scale = S.mul
      end)
      (Core)

  let conj m = Core.map S.conj m

  include Product.Extend (S) (Container.Tup2)
end
