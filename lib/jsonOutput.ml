(** JSON output module for anvil compiler results *)

type json_position = {
  line: int;
  col: int;
}
[@@deriving to_yojson]

type json_trace = {
  path: string option;
  start_pos: json_position [@key "start"];
  end_pos: json_position [@key "end"];
}
[@@deriving to_yojson]

type json_fragment = {
  kind: string;  (* "text" | "codespan" *)
  text: string option;
  trace: json_trace option;
}
[@@deriving to_yojson]

type json_error = {
  error_type: string;  (* "warning" | "error" *)
  path: string option;
  description: json_fragment list;
}
[@@deriving to_yojson]

(** Convert error message to JSON errors *)
let error_message_to_json_errors (error_type : string) (msg : Except.error_message) =
  let open Except in
  let description =
    List.map (function
      | Text desc -> { kind = "text"; text = Some desc; trace = None }
      | Codespan (path, span) ->
        let open Lang in
        let trace = {
          path;
          start_pos = { line = span.st.pos_lnum; col = span.st.pos_cnum - span.st.pos_bol };
          end_pos = { line = span.ed.pos_lnum; col = span.ed.pos_cnum - span.ed.pos_bol };
        } in
        let str =
          match path with
          | None -> None
          | Some filename -> SpanPrinter.string_of_code_span filename span
        in
        { kind = "codespan"; text = str; trace = Some trace }
    ) msg
  in
  let rec find_first_codespan = function
    | [] -> None
    | Codespan (path, span) :: _ -> Some (path, span)
    | _ :: rest -> find_first_codespan rest
  in
  let path = match find_first_codespan msg with
    | None -> None
    | Some (path, _) -> path
  in
  [{ error_type; path; description }]



module Y = Yojson.Safe

let json_output_to_string json_out =
  Y.to_string json_out

(** Create successful JSON output *)
let transpiled_output output_str =
  `Assoc [
    ("success", `Bool true);
    ("errors", `List []);
    ("output", `String output_str)
  ]

(** Create AST output *)
let ast_output ast =
  let convert_compilation_units_to_json ast_out =
    `List (List.map (fun (fname, cunit) ->
      `Assoc [
        ("file_name", `String fname);
        ("compilation_unit", Lang.compilation_unit_to_yojson cunit)
      ]
    ) ast_out)
  in
  `Assoc [
    ("success", `Bool true);
    ("errors", `List []);
    ("output", convert_compilation_units_to_json ast)
  ]

(** Create failed JSON output *)
let failure_output errors =
  `Assoc [
    ("success", `Bool false);
    ("errors", `List (List.map json_error_to_yojson errors));
    ("output", `Null)
  ]
