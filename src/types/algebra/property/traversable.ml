module type S = sig
  include Applicable.S
  include Foldable.S with type 'a t := 'a t
end
