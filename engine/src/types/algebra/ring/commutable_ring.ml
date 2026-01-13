module type S = sig
  include Ring.S
  include Commutable.S
end
