module type S = sig
  include Add_group.S
  include Semi_ring.S with type t := t
end
