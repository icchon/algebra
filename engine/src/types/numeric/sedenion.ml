(* module BASE = struct
  include Cayley_dickson.Make (Octonion)
  include Cayley_dickson.InvExtend (Octonion)

  let v x = x
end

include BASE

include
  Formatter.GenLinear
    (Rational)
    (struct
      type nonrec t = t

      let components x = BASE.flatten x

      let bases _level len =
        List.init len (fun i ->
            if i = 0 then "" else "e_{" ^ string_of_int i ^ "}")
    end)

include Mul_group.Extend (BASE)
include Vector_space.Extend (Octonion) (Container.Tup2) *)
