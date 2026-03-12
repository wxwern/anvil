(** This module defines methods to assist in annotating nodes within the AST *)

let enabled = ref false


let to_def_span (code_span : Lang.code_span) (cunit_fname : string option) : Lang.def_span =
  { st = code_span.st; ed = code_span.ed; cunit = cunit_fname }

let to_code_span (def_span : Lang.def_span) : Lang.code_span =
  { st = def_span.st; ed = def_span.ed }

let merge_def_spans (extra: Lang.def_span list) (base: Lang.def_span list) : Lang.def_span list =
  let append item base =
    if List.exists ((=) item) base then base
    else item :: base
  in List.fold_right append extra base

(** attaches the compilation unit filename to all top-level scoped definitions within the given target compilation unit *)
let attach_def_cunit_fname (target : Lang.compilation_unit) =
  if not !enabled then () else

  let n = target.cunit_file_name in

  let apply (cc: Lang.channel_class_def) = cc.cunit_file_name <- n in
  let _ = List.iter apply target.channel_classes in

  let apply (td: Lang.type_def) = td.cunit_file_name <- n in
  let _ = List.iter apply target.type_defs in

  let apply (md: Lang.macro_def) = md.cunit_file_name <- n in
  let _ = List.iter apply target.macro_defs in

  let apply (fd: Lang.func_def) = fd.cunit_file_name <- n in
  let _ = List.iter apply target.func_defs in

  let apply (pd: Lang.proc_def) = pd.cunit_file_name <- n in
  let _ = List.iter apply target.procs in
  let _ = List.iter apply target._extern_procs in

  let apply (md: Lang.message_def) = md.cunit_file_name <- n in
  let apply_to_chan_msgs (cc: Lang.channel_class_def) = List.iter apply cc.messages in
  let _ = List.iter apply_to_chan_msgs target.channel_classes in

  ()



(** Scoped definition helpers **)
(**
   Dev Notes:

   All definition information should be attached from the closest source node first.

   For example, if x has definition y which has definition z, and z's definition is useful to x,
   then x should have y attached first before z.
  *)

(** attaches definition information to the target (1st arg) from the source (2nd arg) from the given source compilation unit filename (3rd arg) *)
let attach_def_span_expr (target : 'a Lang.ast_node) (source : 'b Lang.ast_node) (source_cunit_fname: string option) =
  if not !enabled then () else

  let base_def_span = source.def_span in
  let source_def_span = to_def_span source.span source_cunit_fname in
  let target_def_span = target.def_span in

  let merged_def_span = merge_def_spans [source_def_span] base_def_span |> merge_def_spans target_def_span in
  target.def_span <- merged_def_span

(** attaches definition information to the target (1st arg) from the source code span (2nd arg) from the given source compilation unit filename (3rd arg) *)
let attach_def_from_code_span (target : 'a Lang.ast_node) (source_span : Lang.code_span) (source_cunit_fname: string option) =
  if not !enabled then () else

  let source_def_span = to_def_span source_span source_cunit_fname in
  target.def_span <- merge_def_spans [source_def_span] target.def_span

(** attaches definition information to the target (1st arg) from the source def span (2nd arg) *)
let attach_def_span (target : 'a Lang.ast_node) (source_span : Lang.def_span) =
  target.def_span <- merge_def_spans [source_span] target.def_span



(** Top-level helpers **)

(** attaches definition information to the target (1st arg) from the source top-level channel_class_def (2nd arg) *)
let attach_def_from_top_level_channel_class (target : 'a Lang.ast_node) (source : Lang.channel_class_def) =
  if not !enabled then () else

  let source_def_span = to_def_span source.span source.cunit_file_name in
  target.def_span <- merge_def_spans [source_def_span] target.def_span

(** attaches definition information to the target (1st arg) from the source top-level type_def (2nd arg) *)
let attach_def_from_top_level_type (target : 'a Lang.ast_node) (source : Lang.type_def) =
  if not !enabled then () else

  let source_def_span = to_def_span source.span source.cunit_file_name in
  target.def_span <- merge_def_spans [source_def_span] target.def_span

(** attaches definition information to the target (1st arg) from the source top-level macro_def (2nd arg) *)
let attach_def_from_top_level_macro (target : 'a Lang.ast_node) (source : Lang.macro_def) =
  if not !enabled then () else

  let source_def_span = to_def_span source.span source.cunit_file_name in
  target.def_span <- merge_def_spans [source_def_span] target.def_span

(** attaches definition information to the target (1st arg) from the source top-level func_def (2nd arg) *)
let attach_def_from_top_level_func (target : 'a Lang.ast_node) (source : Lang.func_def) =
  if not !enabled then () else

  let source_def_span = to_def_span source.span source.cunit_file_name in
  target.def_span <- merge_def_spans [source_def_span] target.def_span

(** attaches definition information to the target (1st arg) from the source top-level proc_def (2nd arg) *)
let attach_def_from_top_level_proc (target : 'a Lang.ast_node) (source : Lang.proc_def) =
  if not !enabled then () else

  let source_def_span = to_def_span source.span source.cunit_file_name in
  target.def_span <- merge_def_spans [source_def_span] target.def_span

(** attaches definition information to the target (1st arg) from the source top-level message_def (2nd arg) *)
let attach_def_from_top_level_message (target : 'a Lang.ast_node) (source : Lang.message_def) (spec: Lang.message_specifier) (graph: EventGraph.event_graph) =
  if not !enabled then () else

  let ep = spec.endpoint in
  let located_defs =
    let is_match (e : Lang.endpoint_def) = e.name = ep in
    List.find_opt is_match (graph.messages.endpoints @ graph.messages.args)
  in
  (
    match located_defs with
    | Some ep ->
      attach_def_from_code_span target ep.span (source.cunit_file_name);
    | _ -> ()
  );

  let source_def_span = to_def_span source.span source.cunit_file_name in
  target.def_span <- merge_def_spans [source_def_span] target.def_span


(** attaches definition information to the fields (1st arg) from the source top-level type_def (2nd arg) *)
let attach_def_from_top_level_type_fields (target_fields: (Lang.identifier * 'a Lang.ast_node) list) (source : Lang.type_def) =
  if not !enabled then () else

  let record_fields = match source.body with
    | `Record fields -> List.map (fun (n: 'c Lang.ast_node) -> (fst n.d, n)) fields
    | _ -> []
  in

  let variant_fields = match source.body with
    | `Variant (_, variants) -> List.map (fun (n: 'd Lang.ast_node) -> let id, _, _ = n.d in (id, n)) variants
    | _ -> []
  in

  let annotator def_fields = (fun (field_ident, field_expr) ->
    match (List.assoc_opt field_ident def_fields) with
    | Some field_type_data ->
        attach_def_from_top_level_type field_expr source;
        attach_def_span_expr field_expr field_type_data source.cunit_file_name
    | None -> ()
  )
  in

  List.iter (annotator record_fields) target_fields;
  List.iter (annotator variant_fields) target_fields;
  ()

(** attaches definition information to the target (1st arg) from the source top-level type_def (2nd arg) and its fields (3rd arg) *)
let attach_def_from_top_level_type_with_fields (target : 'a Lang.ast_node) (source : Lang.type_def) (fields: (Lang.identifier * 'b Lang.ast_node) list) =
  if not !enabled then () else

  attach_def_from_top_level_type target source;
  attach_def_from_top_level_type_fields fields source



(** Event helpers **)
(**
   These methods are used to attach event information to AST nodes.
   They are useful info regarding lifetimes of statements and estimated clock cycles they execute in.
  *)

(** attaches event information to the target (1st arg) from the source (2nd arg), optionally sustained until the given event (3rd arg) *)
let attach_event (target : 'a Lang.ast_node) (source : EventGraph.event) (sustained_until : EventGraph.event option) =
  if not !enabled then () else

  target.action_event <- Some (
    source.graph.thread_id,
    source.id,
    Option.map (fun (e: EventGraph.event) -> e.id) sustained_until
  );
  source.expr_nodes <- target :: source.expr_nodes


(** attaches all events in the graph collection to the correct AST nodes (also in the collection),
    which may be required after event-optimization passes that merge events together *)
let attach_all_events (graph_collection : EventGraph.event_graph_collection) =
  if not !enabled then () else

  let graphs = graph_collection.event_graphs in
  List.iter (fun (graph: EventGraph.proc_graph) ->
    List.iter (fun ((thread, _) : EventGraph.event_graph * 'a option) ->
      let eid_mappings: (int * int) list ref = ref [] in

      List.iter (fun (event: EventGraph.event) ->
        let eid = event.id in
        List.iter (fun (merge_in_eid: int) ->
          eid_mappings := (merge_in_eid, eid) ::
            (List.remove_assoc merge_in_eid !eid_mappings);
        ) event.merged_ids
      ) thread.events;

      (* update event ids *)
      List.iter (fun (event: EventGraph.event) ->
        List.iter (fun (node: 'c Lang.ast_node) ->
          let ae = node.action_event in
          match ae with
          | None -> ()
          | Some (_, prev_eid, ueid) ->
            (* update event id *)
            if prev_eid <> event.id then
              eid_mappings := (prev_eid, event.id) :: (List.remove_assoc prev_eid !eid_mappings);
              node.action_event <- Some (thread.thread_id, event.id, ueid)
        ) event.expr_nodes
      ) thread.events;

      List.iter (fun (event: EventGraph.event) ->
        List.iter (fun (node: 'c Lang.ast_node) ->
          let ae = node.action_event in
          match ae with
          | None -> ()
          | Some (tid, eid, ueid) ->
            (* update until event id *)
            match ueid with
            | None -> ()
            | Some ueid ->
              let new_ueid = List.assoc_opt ueid !eid_mappings in
              Option.iter (fun new_ueid -> node.action_event <- Some (tid, eid, Some new_ueid)) new_ueid

        ) event.expr_nodes
      ) thread.events

    ) graph.threads
  ) graphs



