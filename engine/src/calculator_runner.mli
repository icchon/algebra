open Types

module type RUNNER = sig
  val run : unit -> unit
end

module Make (A : Calculator_field.S) : RUNNER
