(* Dhruv Makwana
 * This file is based off of the dpd_dot.ml
 * TODO: Licensing and Copyright *)

module C = Dpd_compute
module G = Dpd_compute.G
module Node = Dpd_compute.Node

let str_opt_to_str = function 
    | Some v -> v
    | None -> "" (* TODO: Warning *)

let bool_opt_to_bool = function
    | Some true -> true
    | Some false -> false
    | None -> false (* TODO: Warning *)

let print_nodes fmt graph =
  let print_node n = 
    let id, name = Node.id n, Node.name n in
    let kind = str_opt_to_str (Node.get_attrib "kind" n) in 
    let subkind = str_opt_to_str (Node.get_attrib "subkind" n) in 
    let typ = str_opt_to_str (Node.get_attrib "type" n) in 
    let path = str_opt_to_str (Node.get_attrib "path" n) in
    Format.fprintf fmt "%d,%s,%s,%s,%s,%s@." id name kind subkind path typ in

  (* Header, for Neo4j import *)
  Format.fprintf fmt "objectId:ID(Object),name,kind:LABEL,subkind:LABEL,path,type@.";
  G.iter_vertex print_node graph
;;

let print_edges fmt graph =
  let nb_use e = 
    try int_of_string (List.assoc "weight" (G.E.label e))
    with Not_found | Failure _ (* "int_of_string" *) -> 0 in
  let print_edge e = 
    Format.fprintf fmt "%d,%d,%d,%s@."
      (Node.id (G.E.dst e))
      (Node.id
      (G.E.src e))
      (nb_use e)
      (List.assoc "type" (G.E.label e)) in

  (* Header, for Neo4j import *)
  Format.fprintf fmt ":START_ID(Object),:END_ID(Object),weight:int,:TYPE@.";
  G.iter_edges_e print_edge graph
;;

let try_open_default file_prefix =
  let filename = file_prefix ^ ".csv" in
  try open_out filename
  with Sys_error msg ->
    C.warning "cannot open file: %s@." msg;
    let file = Filename.temp_file file_prefix ".dpd" in
    open_out file
;;

let graph_file _ file_prefix g =
  let node_oc = try_open_default (file_prefix ^ "_node") in
  print_nodes (Format.formatter_of_out_channel node_oc) g;
  close_out node_oc;
  let edge_oc = try_open_default (file_prefix ^ "_edge") in
  print_edges (Format.formatter_of_out_channel edge_oc) g;
  close_out edge_oc;
  C.feedback "Graph output in %s@." (file_prefix ^ "_[node|edge].csv");
;;
