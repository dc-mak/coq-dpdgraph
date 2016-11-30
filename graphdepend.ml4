(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
(*            This file is part of the DpdGraph tools.                        *)
(*   Copyright (C) 2009-2015 Anne Pacalet (Anne.Pacalet@free.fr)              *)
(*             ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~                                *)
(*        This file is distributed under the terms of the                     *)
(*         GNU Lesser General Public License Version 2.1                      *)
(*        (see the enclosed LICENSE file for mode details)                    *)
(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

DECLARE PLUGIN "dpdgraph"

open Pp
open Constrarg
open Stdarg

(* Feedback functions *)
let debug msg = if true then Feedback.msg_debug msg

let feedback msg = Feedback.msg_notice (str "Info: " ++ msg)

let warning msg = Feedback.msg_warning (str "Warning: " ++ msg)

let error msg = Feedback.msg_error (str "Error: " ++ msg)

let filename = ref "graph.dpd"

(* Copied from 4.04 version of OCaml stdlib *)
let split_on_char (sep : char) (s : string) : string list =
  let r = ref [] in
  let j = ref (String.length s) in
  for i = String.length s - 1 downto 0 do
    if String.unsafe_get s i = sep then begin
      r := String.sub s (i + 1) (!j - i - 1) :: !r;
      j := i
    end
  done;
  String.sub s 0 !j :: !r

(** Type of Object to track dependencies between *)
(** Graph *)
module G = struct

  module Node = struct

    type obj =
      | Gref of Globnames.global_reference
      | Module of Names.ModPath.t

    type t = int * obj
    let id n = fst n
    let obj n = snd n
    let compare n1 n2 = Pervasives.compare (id n1) (id n2)
    let equal n1 n2 = 0 = compare n1 n2

    let split_name = function
	  | Gref gref ->
		  let dir, id = Libnames.repr_path (Nametab.path_of_global gref) in
		  let dir, name = Names.DirPath.to_string dir, Names.Id.to_string id in
		  ((if dir = "<>" then "" else dir), name)

	  | Module modpath ->
		  let mod_str = Names.DirPath.to_string (Names.ModPath.dp modpath) in
		  let () = assert (mod_str <> "<>") in 
		  match split_on_char '.'  mod_str with
			| [] -> assert false
			| x :: xs -> (String.concat "." xs, x)
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

  type t = (Node.obj, int) Hashtbl.t * Edges.t

  let empty () = Hashtbl.create 10, Edges.empty

  (** new numbers to store global references in nodes *)
  let obj_cpt = ref 0

  let nb_vertex (nds, _eds) = Hashtbl.length nds

  let get_node (nds, eds) obj =
    try Some (Hashtbl.find nds obj, obj)
    with Not_found -> None

  (** *)
  let add_node ((nds, eds) as g) obj = 
    match get_node g obj with
      | Some n -> g, n
      | None ->
          obj_cpt := !obj_cpt + 1; 
          Hashtbl.add nds obj !obj_cpt;
          let n = (!obj_cpt, obj) in
          g, n

  let add_edge (nds, eds) n1 n2 nb = nds, Edges.add (n1, n2, nb) eds

  let iter_vertex fv (nds, _eds) =
    Hashtbl.iter (fun obj id -> fv (id, obj)) nds

  let iter_edges_e fe (_nds, eds) = Edges.iter fe eds

end

(* Add module dependencies recursively *)
let rec add_mod_dpd_rec graph todo parent modpath =
  match modpath with
	| Names.ModPath.MPfile _
	| Names.ModPath.MPbound _ ->
		(match G.get_node graph (G.Node.Module modpath) with
		  | Some node ->
			  let graph = G.add_edge graph parent node 1 in
			  graph, todo

		  | None ->
			  let graph, node = G.add_node graph (G.Node.Module modpath) in
			  let graph = G.add_edge graph parent node 1 in
			  graph, node :: todo)

	| Names.ModPath.MPdot (child, label) ->
		(match G.get_node graph (G.Node.Module modpath) with
		  | Some node ->
			  let graph = G.add_edge graph parent node 1 in
			  add_mod_dpd_rec graph todo node child

		  | None ->
			  let graph, node = G.add_node graph (G.Node.Module modpath) in
			  let graph = G.add_edge graph parent node 1 in
			  add_mod_dpd_rec graph (node :: todo) node child)

(* Add dependencies of modules *)
let add_module_dpds graph all todo modpath =
  let path, name = G.Node.split_name (G.Node.Module modpath) in
  let () = debug (str "Add module dpds " ++ str (path ^ "." ^ name)) in
  (match modpath with
	| Names.ModPath.MPfile _
	| Names.ModPath.MPbound _ ->
		(match G.get_node graph (G.Node.Module modpath) with
		  | Some _ ->
			  graph, todo
		  | None ->
			  let g, n = G.add_node graph (G.Node.Module modpath) in
			  g, n :: todo)
(* ???
			  if all then
				let g, n = G.add_node graph (G.Node.Module modpath) in
				g, n :: todo
			  else
				graph, todo)
*)

	| Names.ModPath.MPdot (child, _) ->
		(match G.get_node graph (G.Node.Module modpath) with
		  | Some node ->
			  add_mod_dpd_rec graph todo node child
		  | None ->
			  let graph, node = G.add_node graph (G.Node.Module modpath) in
			  add_mod_dpd_rec graph (node :: todo) node child))
(* ???
			  if all then
				let graph, node = G.add_node graph (G.Node.Module modpath) in
				add_mod_dpd_rec (node :: todo) graph node child
			  else 
				 graph, todo))
*)

let add_gref_module graph all todo gref =
  let dir = Nametab.dirpath_of_global gref in
  let qualid = Libnames.qualid_of_dirpath dir in
  let modpath = Nametab.locate_module qualid in
  let graph, todo = add_module_dpds graph all todo modpath in
  match G.get_node graph (G.Node.Module modpath),
        G.get_node graph (G.Node.Gref gref) with

  | Some modp, Some gref ->
      let graph = G.add_edge graph modp gref 1 in
      graph, todo

  | None, Some gref ->
      let graph, modp = G.add_node graph (G.Node.Module modpath) in
      let graph = G.add_edge graph modp gref 1 in
      graph, todo 

  | Some modp, None -> 
      let graph, gref = G.add_node graph (G.Node.Gref gref) in
      let graph = G.add_edge graph modp gref 1 in
      graph, todo 

  | None, None ->
      let graph, gref = G.add_node graph (G.Node.Gref gref) in
      let graph, modp = G.add_node graph (G.Node.Module modpath) in
      let graph = G.add_edge graph modp gref 1 in
      graph, todo 

(** add the dependencies of obj in the graph (obj is already in).
  * If [all], add also the nodes of the dependencies that are not in,
  * and return the list of the new nodes,
  * If not all, don't add nodes, and return an empty list. *)
let add_obj_dpds graph ~all n_obj todo =
  match G.Node.obj n_obj with
  | G.Node.Module modpath ->
	  add_module_dpds graph all todo modpath

  | G.Node.Gref gref ->
    let () = debug (str "Add dpds " ++ Printer.pr_global gref) in
    let add_dpd dpd nb_use (g, td) =
	  (match G.get_node g (G.Node.Gref dpd) with
      | Some n ->
          let g = G.add_edge g n_obj n nb_use in
          g, td

      | None -> 
          if all then 
            let g, n = G.add_node g (G.Node.Gref dpd) in
            let g = G.add_edge g n_obj n nb_use in
			g, n::td
          else
            g, td) in
      try
        let data = Searchdepend.collect_dependance gref in
        let graph, todo = add_gref_module graph all todo gref in
        let graph, todo = Searchdepend.Data.fold add_dpd data (graph, todo) in
		graph, todo
      with Searchdepend.NoDef gref ->
        graph, todo (* nothing to do *)

(** add obj node and add it to the todo list 
 * to process its dependencies later. *)
let add_obj_only (graph, todo) obj = 
  let path, name = G.Node.split_name obj in
  let () = debug (str "Add " ++ str (path ^ "." ^ name)) in
  let graph, n = G.add_node graph obj in
  graph, n :: todo

(** add the obj in [l] and build the dependencies according to [all] *)
let add_obj_list_and_dpds graph ~all l =
  let graph, todo = List.fold_left add_obj_only (graph, []) l in 
  let rec add_obj_dpds_rec graph todo = match todo with
    | [] -> graph
    | n::todo -> 
        let graph, todo = add_obj_dpds graph ~all n todo in
		add_obj_dpds_rec graph todo in

  add_obj_dpds_rec graph todo

(** Don't forget to update the README file if something is changed here *)
module Out : sig
  val file : G.t -> unit
end = struct

  let type_of_constref =
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

  let get_constr_type typ =
    Names.KerName.to_string (Names.MutInd.user typ)

  let type_of_gref gref = 
	if Typeclasses.is_class gref then
	  "class"
	else
	  match gref with
	  | Globnames.ConstRef cst ->
		type_of_constref (Decls.constant_kind cst)

	  | Globnames.ConstructRef ((typ, _), _) -> 
		"construct"

	  | Globnames.IndRef ind -> 
		type_of_ind ind

	  | Globnames.VarRef _ ->
		assert false


  let type_of_obj = function
	| G.Node.Gref gref -> 
		type_of_gref gref
	| G.Node.Module modpath ->
        if Names.ModPath.is_bound modpath then "mod" else "file"

  let pp_attribs fmt attribs =
      List.iter (fun (a,b) -> Format.fprintf fmt "%s=%s, " a b) attribs

  let out_node fmt g n = 
    let id, obj = G.Node.id n, G.Node.obj n in
    let dirname, name = G.Node.split_name obj in
    let acc = if dirname = "" then [] else [("path", "\""^dirname^"\"")] in
    let acc = ("kind", type_of_obj obj) :: acc in
    Format.fprintf fmt "N: %d \"%s\" [%a];@." id name pp_attribs acc

  let out_edge fmt _g e =

    let matches typ n =
      let dirname, name = G.Node.split_name (G.Node.obj n) in
      get_constr_type typ = dirname ^ "." ^ name in

    (* incorporate src & dst types, flip if constructor & ind & match
     * TO FIX WRONG WAY DEPENDENCY LINK FROM SEARCHDEPEND.ML4 *)
     let src, dst =
      let src, dst = G.Edge.src e, G.Edge.dst e in
      match G.Node.obj src, G.Node.obj dst with
      | G.Node.Gref (Globnames.ConstructRef ((typ, _), _)),
        G.Node.Gref (Globnames.IndRef _) when matches typ dst ->
        dst, src
      | _, _ ->
        src, dst in

    let edge_type =
      type_of_obj (G.Node.obj src)
      ^ "_USED_BY_"
      ^ type_of_obj (G.Node.obj dst) in

    let edge_attribs =
      [ ("type", edge_type) ; ("weight", string_of_int (G.Edge.nb_use e))] in

    Format.fprintf fmt "E: %d %d [%a];@."
        (G.Node.id src)
        (G.Node.id dst)
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
  let graph = add_obj_list_and_dpds graph ~all [G.Node.Gref gref] in
  Out.file graph

let get_dirlist_grefs dirlist =
  let selected_gref = ref [] in
  let select gref env constr = 
    if Search.module_filter (dirlist, false) gref env constr then 
    (debug (str "Select " ++ Printer.pr_global gref);
     selected_gref := gref::!selected_gref)
  in 
    Search.generic_search None select;
    !selected_gref

let file_graph_depend dirlist =
  let graph = G.empty () in
  let grefs = get_dirlist_grefs dirlist in
  let all = false in (* then add the dependencies only to existing nodes *)
  let graph = add_obj_list_and_dpds graph ~all (List.map (fun x -> G.Node.Gref x) grefs) in
  Out.file graph

let locate_mp_dirpath ref =
  let (loc,qid) = Libnames.qualid_of_reference ref in
  try Nametab.dirpath_of_module (Nametab.locate_module qid)
  with Not_found ->
    CErrors.user_err_loc
      (loc,"",str "Unknown module" ++ spc() ++ Libnames.pr_qualid qid)

VERNAC COMMAND EXTEND DependGraphSetFile CLASSIFIED AS QUERY
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

VERNAC COMMAND EXTEND DependGraph CLASSIFIED AS QUERY
  | ["Print" "DependGraph" reference(ref) ] ->
      [ mk_dpds_graph (Nametab.global ref) ]
  | ["Print" "FileDependGraph" reference_list(dl) ] ->
      [ file_graph_depend (List.map locate_mp_dirpath dl) ]
END
