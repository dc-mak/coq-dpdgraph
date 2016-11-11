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
        let body = bool_opt_to_bool (Node.bool_attrib "body" n)
        and prop = bool_opt_to_bool (Node.bool_attrib "prop" n) in 
        let  kind = let k = str_opt_to_str (Node.get_attrib "kind" n)
                    in 
(* kind = cnst: 
 
      body,  prop 
      ----------
      true,  true  -> proved lemma
      true,  false -> Definition, etc
      false, true  -> axiom/admitted lemma
      false, false -> Parameter, etc
*) 
                       if k = "cnst" 
                       then if body 
                            then if prop 
                                 then "lemma" 
                                 else "definition" 
                            else if prop 
                                 then "admitted" 
                                 else "parameter" 
                       else k 

        and path = str_opt_to_str (Node.get_attrib "path" n) in
(*
        Format.fprintf fmt "%d,%s,%B,%s,%B,%s@." id name body kind prop path in
*) 
        Format.fprintf fmt "%d,%s,%s,%s@." id name kind path in
(*
    Format.fprintf fmt "objectId:ID(Object),name,body,:LABEL,prop,path@.";
*) 
    Format.fprintf fmt "objectId:ID(Object),name,:LABEL,path@.";
    G.iter_vertex print_node graph

let print_edges fmt graph =
    let nb_use e = 
        try
            int_of_string (List.assoc "weight" (G.E.label e))
        with Not_found | Failure _ (* "int_of_string" *) -> 0 in
    let print_edge e = 
        Format.fprintf fmt "%d,%d,%d,ARC@."
            (Node.id (G.E.src e)) (Node.id (G.E.dst e)) (nb_use e) in
    Format.fprintf fmt ":START_ID(Object),:END_ID(Object),weight,:TYPE@.";
    G.iter_edges_e print_edge graph

let try_open_default file_prefix =
    let filename = file_prefix ^ ".csv" in
    try open_out filename
    with Sys_error msg ->
      C.warning "cannot open file: %s@." msg;
      let file = Filename.temp_file file_prefix ".dpd" in
        open_out file

let graph_file file_prefix g =
  let node_oc = try_open_default (file_prefix ^ "_node") in
  let edge_oc = try_open_default (file_prefix ^ "_edge") in
  C.feedback "Graph output in %s@." (file_prefix ^ "_[node|edge].csv");
  print_nodes (Format.formatter_of_out_channel node_oc) g;
  close_out node_oc;
  print_edges (Format.formatter_of_out_channel edge_oc) g;
  close_out edge_oc

