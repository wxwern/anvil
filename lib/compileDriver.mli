(** Driver that controls the overall compilation process, handling things include file importing. *)

(** Containing file name, code span, and error message. *)
exception CompileError of Except.error_message

val parse : Config.compile_config -> (string * Lang.compilation_unit) list

val compile : out_channel -> Config.compile_config -> unit