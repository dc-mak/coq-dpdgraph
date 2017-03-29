(* Dhruv Makwana - temporary!!! 
 * Needs to be refactored ASAP into a more general
 *      dpd2 [dot | csv | ...]
 * This file is based off of the dpd2dot.ml
 * TODO: Licensing and Copyright *)

let version_option = ref false

type output = CSV | DOT
let output_of_str str =
  match String.(lowercase_ascii (trim str)) with
  | "csv" -> Some CSV
  | "dot" -> Some DOT
  | _ -> None

let out_file = ref None
let set_out_file file = out_file := Some file

let spec_args = [
  ("-o", Arg.String set_out_file, 
      ": name of output file (default: name of input file .<ext>)");
  ("-with-defs", Arg.Set Dpd_compute.with_defs, 
      ": show everything (default)");
  ("-without-defs", Arg.Clear Dpd_compute.with_defs, 
      ": show only Prop objects");
  ("-rm-trans", Arg.Set Dpd_compute.reduce_trans, 
      ": remove transitive dependencies (default)");
  ("-keep-trans", Arg.Clear Dpd_compute.reduce_trans, 
      ": keep transitive dependencies");
  ("-debug", Arg.Set Dpd_compute.debug_flag, 
      ": set debug mode");
  ("-v", Arg.Set version_option, 
      ": print version and exit");
]

let do_file n out_file_type f =
  try
    Dpd_compute.feedback "read file %s@." f;
    let g = Dpd_lex.read f in
    let g = Dpd_compute.build_graph g in
    Dpd_compute.simplify_graph g;
    match output_of_str out_file_type with
    | None -> Dpd_compute.error "unknown output file-format@."
    | Some output ->
    let ext = match output with DOT -> ".dot" | CSV -> ".csv" in
    let file = match !out_file with 
      | None -> (Filename.chop_extension f)^ext
      | Some f -> 
          if n = 0 then f 
          else (Filename.chop_extension f)^"."^(string_of_int n)^ext
    in match output with
    (*| DOT -> Dpd_dot.graph_file file g*)
      | DOT -> failwith "dot output unsupported"
      | CSV -> Dpd_csv.graph_file file g
  with Dpd_compute.Error msg -> Dpd_compute.error "%s@." msg

let main () =
  let usage_msg = "Usage : "^(Sys.argv.(0))^"<csv|dot> [options]" in
  let args = ref [] in
  let memo_arg arg = args := arg :: !args in
    Arg.parse spec_args memo_arg usage_msg;
    if !version_option 
    then Format.printf "This is '%s' (part of DpdGraph tools - version %s)@."
      (Filename.basename Sys.argv.(0)) Version.version
    else match List.rev (!args) with
      | [] | [_] ->
          Dpd_compute.error "dpd2 <csv|dot> <file>.@";
          Arg.usage spec_args usage_msg
      | [out_file_type; f] ->
          do_file 0 out_file_type f
      | out_file_type :: l  -> 
          ignore (List.fold_left (fun n f -> do_file n out_file_type f; n+1) 1 l)

let () = main ()
