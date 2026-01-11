module type S = sig
  include Division_ring.S
  include Commutable.S
end
