(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
(*            This file is part of the DpdGraph tools.                        *)
(*   Copyright (C) 2009-2015 Anne Pacalet (Anne.Pacalet@free.fr)           *)
(*             ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~                                *)
(*        This file is distributed under the terms of the                     *)
(*         GNU Lesser General Public License Version 2.1                      *)
(*        (see the enclosed LICENSE file for mode details)                    *)
(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

DECLARE PLUGIN "dpdgraph"

open Pp

let debug msg = if false then Pp.msgnl msg

let feedback msg = Pp.msgnl (str "Info: " ++ msg)

let warning msg = Pp.msgerrnl (str "Warning: " ++ msg)

let error msg = Pp.msgerrnl (str "Error: " ++ msg)

let filename = ref "graph.dpd"

let get_dirlist_grefs dirlist =
  let selected_gref = ref [] in
  let select gref env constr = 
    if Search.module_filter (dirlist, false) gref env constr then 
    (debug (str "Select " ++ Printer.pr_global gref);
     selected_gref := gref::!selected_gref)
  in 
    Search.generic_search None select;
    !selected_gref

module G = struct

  module Node = struct
    type t = int * Globnames.global_reference
    let id n = fst n
    let gref n = snd n
    let compare n1 n2 = Pervasives.compare (id n1) (id n2)
    let equal n1 n2 = 0 = compare n1 n2

    let full_name n = 
      let qualid = 
        Nametab.shortest_qualid_of_global Names.Idset.empty (gref n)
      in Libnames.string_of_qualid qualid

    let split_name n =
      let glob_ref = gref n in
      let dirpath = Names.DirPath.to_string (Nametab.dirpath_of_global glob_ref)
      and name = Names.Id.to_string (Nametab.basename_of_global glob_ref) in
      ((if dirpath = "<>" then "" else dirpath), name)

  end

  module Edge = struct
    type t = Node.t * Node.t * int
    let src (n1, _n2, _nb) = n1
    let dst (_n1, n2, _nb) = n2
    let nb_use (_n1, _n2, nb) = nb
    let compare e1 e2 =
      let cmp_src = Node.compare (src e1) (src e2) in
        if cmp_src = 0 then Node.compare (dst e1) (dst e2) else cmp_src
  end

  module Edges = Set.Make (Edge)

  type t = (Globnames.global_reference, int) Hashtbl.t * Edges.t

  let empty () = Hashtbl.create 10, Edges.empty

  (** new numbers to store global references in nodes *)
  let gref_cpt = ref 0

  let nb_vertex (nds, _eds) = Hashtbl.length nds

  let get_node (nds, eds) gref =
    try Some (Hashtbl.find nds gref, gref)
    with Not_found -> None

  (** *)
  let add_node ((nds, eds) as g) gref = 
    match get_node g gref with
      | Some n -> g, n
      | None ->
          gref_cpt := !gref_cpt + 1; 
          Hashtbl.add nds gref !gref_cpt;
          let n = (!gref_cpt, gref) in
            g, n

  let add_edge (nds, eds) n1 n2 nb = nds, Edges.add (n1, n2, nb) eds

  (* let succ (_nds, eds) n = 
    let do_e e acc = 
      if Node.equal n (Edge.src e) then (Edge.dst e)::acc else acc 
    in Edges.fold do_e eds []

  let pred (_nds, eds) n = 
    let do_e e acc = 
      if Node.equal n (Edge.dst e) then (Edge.src e)::acc else acc 
    in Edges.fold do_e eds []
   *)

  let iter_vertex fv (nds, _eds) =
    Hashtbl.iter (fun gref id -> fv (id, gref)) nds

  let iter_edges_e fe (_nds, eds) = Edges.iter fe eds
end

(** add the dependencies of gref in the graph (gref is already in).
  * If [all], add also the nodes of the dependancies that are not in,
  * and return the list of the new nodes,
  * If not all, don't add nodes, and return an empty list. *)
let add_gref_dpds graph ~all n_gref todo =
  let gref = G.Node.gref n_gref in
  debug (str "Add dpds " ++ Printer.pr_global gref);
  let add_dpd dpd nb_use (g, td) = match G.get_node g dpd with
    | Some n -> let g = G.add_edge g n_gref n nb_use in g, td
    | None -> 
        if all then 
          let g, n = G.add_node g dpd in
          let g = G.add_edge g n_gref n nb_use in
            g, n::td
        else g, td
  in
    try
      let data = Searchdepend.collect_dependance gref in
      let graph, todo = Searchdepend.Data.fold add_dpd data (graph, todo) in
        graph, todo
    with Searchdepend.NoDef gref -> (* nothing to do *) graph, todo

(** add gref node and add it to the todo list 
* to process its dependencies later. *)
let add_gref_only (graph, todo) gref = 
  debug (str "Add " ++ Printer.pr_global gref);
  let graph, n = G.add_node graph gref in
  let todo = n::todo in
    graph, todo

(** add the gref in [l] and build the dependencies according to [all] *)
let add_gref_list_and_dpds graph ~all l =
  let graph, todo = List.fold_left add_gref_only (graph, []) l in 
  let rec add_gref_dpds_rec graph todo = match todo with
    | [] -> graph
    | n::todo -> 
        let graph, todo = add_gref_dpds graph ~all n todo in
          add_gref_dpds_rec graph todo
  in
  let graph = add_gref_dpds_rec graph todo in
    graph

(** Don't forget to update the README file if something is changed here *)
module Out : sig 
  val file : G.t -> unit 
end = struct

  let add_construct_attrib acc typ =
    let type_str = Names.KerName.to_string (Names.MutInd.user typ) in
    let () = debug (str "Added type_str: " ++ str type_str) in
    ("type", "\"" ^ type_str ^ "\"" ) :: acc

  let type_of_logical_kind =
	let open Decl_kinds in function
	| IsDefinition def ->
		(match def with
		| Definition -> "def"
		| Coercion -> "coe"
		| SubClass -> "subclass"
		| CanonicalStructure -> "canonstruc"
		| Example -> "ex"
		| Fixpoint -> "def"
		| CoFixpoint -> "def"
		| Scheme -> "scheme"
		| StructureComponent -> "proj"
		| IdentityCoercion -> "coe"
		| Instance -> "inst"
		| Method -> "meth")
	| IsAssumption a ->
		(match a with
		| Definitional -> "defax"
		| Logical -> "prfax"
		| Conjectural -> "prfax")
	| IsProof th ->
		(match th with
		| Theorem
		| Lemma
		| Fact
		| Remark
		| Property
		| Proposition
		| Corollary -> "thm")

  let type_of_ind ind =
	let (mib,oib) = Inductive.lookup_mind_specif (Global.env ()) ind in
	if mib.Declarations.mind_record <> None then
	  let open Decl_kinds in
	  begin match mib.Declarations.mind_finite with
	  | Finite -> "indrec"
	  | BiFinite -> "rec"
	  | CoFinite -> "corec"
	  end
	else
	  let open Decl_kinds in
	  begin match mib.Declarations.mind_finite with
	  | Finite -> "ind"
	  | BiFinite -> "variant"
	  | CoFinite -> "coind"
	  end

  let add_gref_attrib acc gref id =
	if Typeclasses.is_class gref then
	  ("kind", "class") :: acc
	else
	  match gref with
	  | Globnames.ConstRef cst ->
		  let kind = type_of_logical_kind (Decls.constant_kind cst) in
		  ("kind", kind) :: acc

	  | Globnames.ConstructRef ((typ, _), _) -> 
		let acc = ("kind", "construct")::acc in
		add_construct_attrib acc typ

	  | Globnames.IndRef ind -> 
		("kind", type_of_ind ind) :: acc	

	  | Globnames.VarRef _ ->
		assert false

  let pp_attribs fmt attribs =
      List.iter (fun (a,b) -> Format.fprintf fmt "%s=%s, " a b) attribs

  let out_node fmt g n =
    let id = G.Node.id n in
    let gref = G.Node.gref n in
    let dirname, name = G.Node.split_name n in
    let acc = if dirname = "" then [] else [("path", "\""^dirname^"\"")] in
    let acc = add_gref_attrib acc gref name in
      Format.fprintf fmt "N: %d \"%s\" [%a];@." id name
        pp_attribs acc

  let out_edge fmt _g e =
    let edge_attribs = ("weight", string_of_int (G.Edge.nb_use e))::[] in
    Format.fprintf fmt "E: %d %d [%a];@." 
      (G.Node.id (G.Edge.src e)) (G.Node.id (G.Edge.dst e))
      pp_attribs edge_attribs

  let out_graph fmt g =
    G.iter_vertex (out_node fmt g) g;
    G.iter_edges_e (out_edge fmt g) g

  let file graph =
    try
      let oc = open_out !filename  in
        feedback (str "output dependencies in file " ++ (str !filename));
        out_graph (Format.formatter_of_out_channel oc) graph;
        close_out oc
    with Sys_error msg ->
      error (str "cannot open file: " ++ (str msg));
end
  
let mk_dpds_graph gref =
  let graph = G.empty () in 
  let all = true in (* get all the dependencies recursively *)
  let graph = add_gref_list_and_dpds graph ~all [gref] in
    Out.file graph

let file_graph_depend dirlist = 
  let graph = G.empty () in
  let grefs = get_dirlist_grefs dirlist in
  let all = false in (* then add the dependencies only to existing nodes *)
  let graph = add_gref_list_and_dpds graph ~all grefs in
    Out.file graph

let locate_mp_dirpath ref =
  let (loc,qid) = Libnames.qualid_of_reference ref in
  try Nametab.dirpath_of_module (Nametab.locate_module qid)
  with Not_found -> 
    Errors.user_err_loc 
      (loc,"",str "Unknown module" ++ spc() ++ Libnames.pr_qualid qid)

VERNAC COMMAND EXTEND DependGraphSetFile
  | ["Set" "DependGraph" "File" string(str)] -> [ filename := str ]
END

(*
VERNAC ARGUMENT EXTEND dirpath
  [ string(str) ] -> [ Globnames.dirpath_of_string str ]
END

VERNAC ARGUMENT EXTEND dirlist
  | [ dirpath(d) dirlist(l)] -> [ d::l ]
  | [ dirpath(d) ] -> [ [d] ]
END
*)

VERNAC COMMAND EXTEND DependGraph
  | ["Print" "DependGraph" reference(ref) ] -> 
      [ mk_dpds_graph (Nametab.global ref) ]
  | ["Print" "FileDependGraph" reference_list(dl) ] -> 
      [ file_graph_depend (List.map locate_mp_dirpath dl) ]
END


