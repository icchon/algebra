module type S = sig
  include Add_monoid.S
  include Mul_monoid.S with type t := t
end
