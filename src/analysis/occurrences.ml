open Std
module Lid_set = Index_format.Lid_set

let { Logger.log } = Logger.for_section "occurrences"

type t =
  { occurrences : Query_protocol.occurrence list;
    status : Query_protocol.occurrences_status
  }

module Staleness = struct
  type t = Stale | Fresh

  let is_stale = function
    | Stale -> true
    | Fresh -> false
end

module Occurrence_set : sig
  type t

  val empty : t

  (** Filter an [Lid_set.t]. [Lid.t]s that are kept must be assigned a staleness *)
  val of_filtered_lid_set :
    Lid_set.t -> f:(Index_format.Lid.t -> Staleness.t option) -> t

  val to_list : t -> (Index_format.Lid.t * Staleness.t) list
  val union : t -> t -> t
end = struct
  module Lid_map = Map.Make (Index_format.Lid)

  type t = Staleness.t Lid_map.t

  let empty = Lid_map.empty
  let to_list = Lid_map.to_list

  let of_filtered_lid_set lid_set ~f:get_staleness =
    let maybe_add_lid lid acc =
      match get_staleness lid with
      | Some staleness -> Lid_map.add lid staleness acc
      | None -> acc
    in
    Lid_set.fold maybe_add_lid lid_set empty

  let either_fresh a b =
    let open Staleness in
    match (a, b) with
    | Fresh, _ | _, Fresh -> Fresh
    | Stale, Stale -> Stale

  let union a b = Lid_map.union (fun _ a b -> Some (either_fresh a b)) a b
end

let () = Mtyper.set_index_items Index_occurrences.items

let set_fname ~file (loc : Location.t) =
  let pos_fname = file in
  { loc with
    loc_start = { loc.loc_start with pos_fname };
    loc_end = { loc.loc_end with pos_fname }
  }

(* Merlin-jst: Upstream Merlin only includes the location of the last segment of an
   identifier. (ex: If the user wrote "Foo.bar", only the location of "bar") is included.
   We instead choose to include the entire "Foo.bar", for two reasons:
   1. We think that this is a slightly better user experience
   2. Upstream Merlin does not include occurrences within ppx generated code, but we do.
      Because of this, it is not always true for us that the reported location in the
      buffer is text corresponding to the identifier. (For example, the location may
      correspond to a ppx extension node.) In such a case, attempting to modify the
      location to only include the last segment of the identifier is nonsensical. Since we
      don't have a way to detect such a case, it forces us to not try. *)
(*
(* A longident can have the form: A.B.x Right now we are only interested in
   values, but we will eventually want to index all occurrences of modules in
   such longidents. However there is an issue with that: we only have the
   location of the complete longident which might span multiple lines. This is
   enough to get the last component since it will always be on the last line,
   but will prevent us to find the location of previous components.

   However, we can safely deduce the location of the last part of the lid only
   when the ident does not require parenthesis. In that case the loc sie differs
   from the name size in a way that depends on the concrete syntax which is
   lost. *)
let last_loc (loc : Location.t) lid =
  match lid with
  | Longident.Lident _ -> loc
  | _ ->
    let last_segment = Longident.last lid in
    let needs_parens = Pprintast.needs_parens last_segment in
    if not needs_parens then
      let last_size = last_segment |> String.length in
      { loc with
        loc_start =
          { loc.loc_end with pos_cnum = loc.loc_end.pos_cnum - last_size }
      }
    else loc
*)

let uid_and_loc_of_node env node =
  let open Browse_raw in
  log ~title:"occurrences" "Looking for uid of node %s" @@ string_of_node node;
  match node with
  | Module_binding_name { mb_id = Some ident; mb_name; _ } ->
    let md = Env.find_module (Pident ident) env in
    Some (md.md_uid, mb_name.loc)
  | Pattern
      { pat_desc =
          Tpat_var (_, name, uid, _) | Tpat_alias (_, _, name, uid, _, _);
        _
      } -> Some (uid, name.loc)
  | Type_declaration { typ_type; typ_name; _ } ->
    Some (typ_type.type_uid, typ_name.loc)
  | Label_declaration { ld_uid; ld_name; _ } -> Some (ld_uid, ld_name.loc)
  | Constructor_declaration { cd_uid; cd_name; _ } -> Some (cd_uid, cd_name.loc)
  | Value_description { val_val; val_name; _ } ->
    Some (val_val.val_uid, val_name.loc)
  | _ -> None

let rec comp_unit_of_uid = function
  | Shape.Uid.Compilation_unit comp_unit | Item { comp_unit; _ } ->
    Some comp_unit
  | Unboxed_version uid -> comp_unit_of_uid uid
  | Internal | Predef _ -> None

module Stat_check : sig
  type t
  val create : cache_size:int -> Index_format.index -> t
  val check : t -> file:string -> bool
  val get_outdated_files : t -> String.Set.t
end = struct
  type t = { index : Index_format.index; cache : (string, bool) Hashtbl.t }

  let create ~cache_size index = { index; cache = Hashtbl.create cache_size }

  let get_outdated_files t =
    Hashtbl.fold
      (fun file check acc -> if check then acc else String.Set.add file acc)
      t.cache String.Set.empty

  let stat t file =
    let open Index_format in
    match Stats.find_opt file t.index.stats with
    | None ->
      log ~title:"stat_check" "No stats found for file %S." file;
      true
    | Some { size; _ } -> (
      try
        let stats = Unix.stat file in
        let equal =
          (* This is fast but approximative. A better option would be to check
             [mtime] and then [source_digest] if the times differ. *)
          Int.equal stats.st_size size
        in
        if not equal then
          log ~title:"stat_check"
            "File %s has been modified since the index was built." file;
        equal
      with Unix.Unix_error _ ->
        log ~title:"stat_check" "Could not stat file %S" file;
        false)

  let check t ~file =
    let cache_and_return b =
      Hashtbl.add t.cache file b;
      b
    in
    match Hashtbl.find_opt t.cache file with
    | Some result -> result
    | None -> cache_and_return (stat t file)
end

let get_buffer_locs result uid =
  Stamped_hashtable.fold
    (fun (uid', loc) () acc ->
      if Shape.Uid.equal uid uid' then Lid_set.add loc acc else acc)
    (Mtyper.get_index result) Lid_set.empty

let is_in_interface (config : Mconfig.t) (loc : Warnings.loc) =
  let extension = Filename.extension loc.loc_start.pos_fname in
  List.exists config.merlin.suffixes ~f:(fun (_impl, intf) ->
      String.equal extension intf)

let locs_of ~config ~env ~typer_result ~pos ~scope path =
  log ~title:"occurrences" "Looking for occurences of %s (pos: %s)" path
    (Lexing.print_position () pos);
  let local_defs = Mtyper.get_typedtree typer_result in
  let locate_result =
    Locate.from_string
      ~config:{ mconfig = config; traverse_aliases = false; ml_or_mli = `ML }
      ~env ~local_defs ~pos path
  in
  (* When we fail to find an exact definition we restrict scope to `Buffer *)
  let def, scope =
    match locate_result with
    | `At_origin ->
      log ~title:"locs_of" "Cursor is on definition / declaration";
      (* We are on  a definition / declaration so we look for the node's uid  *)
      let browse = Mbrowse.of_typedtree local_defs in
      let env, node = Mbrowse.leaf_node (Mbrowse.enclosing pos [ browse ]) in
      let node_uid_loc = uid_and_loc_of_node env node in
      let scope =
        match node_uid_loc with
        | Some (_, l) when is_in_interface config l ->
          (* There is no way to distinguish uids from interfaces from uids of
             implementations. We fallback on buffer occurrences in that case.
             TODO: we should be able to improve on that situation when we will be
             able to distinguish between impl/intf uids and know which declaration
             are actually linked. *)
          `Buffer
        | _ -> scope
      in
      (node_uid_loc, scope)
    | `Found { uid; location; approximated = false; _ } ->
      log ~title:"locs_of" "Found definition uid using locate: %a " Logger.fmt
        (fun fmt -> Shape.Uid.print fmt uid);
      (* There is no way to distinguish uids from interfaces from uids of
         implementations. We fallback on buffer occurrences in that case.
         TODO: we should be able to improve on that situation when we will be
         able to distinguish between impl/intf uids and know which declaration
         are actually linked. *)
      let scope = if is_in_interface config location then `Buffer else scope in
      (Some (uid, location), scope)
    | `Found { decl_uid; location; approximated = true; _ } ->
      log ~title:"locs_of" "Approx. definition: %a " Logger.fmt (fun fmt ->
          Shape.Uid.print fmt decl_uid);
      (Some (decl_uid, location), `Buffer)
    | `Builtin (uid, s) ->
      log ~title:"locs_of" "Locate found a builtin: %s" s;
      (Some (uid, Location.none), scope)
    | _ ->
      log ~title:"locs_of" "Locate failed to find a definition.";
      (None, `Buffer)
  in
  let current_buffer_path =
    Filename.concat config.query.directory config.query.filename
  in
  match def with
  | Some (def_uid, def_loc) ->
    log ~title:"locs_of" "Definition has uid %a (%a)" Logger.fmt
      (fun fmt -> Shape.Uid.print fmt def_uid)
      Logger.fmt
      (fun fmt -> Location.print_loc fmt def_loc);
    log ~title:"locs_of" "Indexing current buffer";
    let buffer_locs = get_buffer_locs typer_result def_uid in
    let buffer_occurrences =
      Occurrence_set.of_filtered_lid_set buffer_locs ~f:(fun _ -> Some Fresh)
    in
    let external_occurrences =
      if scope = `Buffer then []
      else
        List.filter_map config.merlin.index_files ~f:(fun index_file ->
            let external_locs =
              try
                let external_index = Index_cache.read index_file in
                Index_format.Uid_map.find_opt def_uid external_index.defs
                |> Option.map ~f:(fun uid_locs -> (external_index, uid_locs))
              with Index_format.Not_an_index _ | Sys_error _ ->
                log ~title:"external_index" "Could not load index %s" index_file;
                None
            in
            Option.map external_locs ~f:(fun (index, locs) ->
                let stats = Stat_check.create ~cache_size:128 index in
                ( Occurrence_set.of_filtered_lid_set locs
                    ~f:(fun ({ loc; _ } as lid) ->
                      (* We filter external results that concern the current buffer *)
                      let file_rel_to_root =
                        loc.Location.loc_start.Lexing.pos_fname
                      in
                      let file_uncanon, buf_uncanon =
                        match config.merlin.source_root with
                        | Some root ->
                          ( Filename.concat root file_rel_to_root,
                            current_buffer_path )
                        | None -> (file_rel_to_root, config.query.filename)
                      in
                      let file = Misc.canonicalize_filename file_uncanon in
                      let buf = Misc.canonicalize_filename buf_uncanon in
                      let is_current_buffer = String.equal file buf in
                      let should_be_ignored =
                        (* We ignore results that don't have a location *)
                        Index_occurrences.should_ignore_lid lid
                      in
                      if is_current_buffer || should_be_ignored then None
                      else begin
                        (* We ignore external results if their source was modified *)
                        let is_fresh =
                          Stat_check.check stats ~file:file_rel_to_root
                        in
                        if not is_fresh then
                          log ~title:"locs_of" "File %s might be out-of-sync."
                            file;
                        let staleness : Staleness.t =
                          match is_fresh with
                          | true -> Fresh
                          | false -> Stale
                        in
                        Some staleness
                      end),
                  Stat_check.get_outdated_files stats )))
    in
    let external_occurrences, out_of_sync_files =
      List.fold_left
        ~init:(Occurrence_set.empty, String.Set.empty)
        ~f:(fun (acc_locs, acc_files) (locs, files) ->
          (Occurrence_set.union acc_locs locs, String.Set.union acc_files files))
        external_occurrences
    in
    let occurrences =
      Occurrence_set.union buffer_occurrences external_occurrences
    in
    let occurrences = Occurrence_set.to_list occurrences in
    log ~title:"occurrences" "Found %i locs" (List.length occurrences);
    let occurrences =
      List.filter_map occurrences
        ~f:(fun (({ txt; loc } : _ Location.loc), staleness) ->
          (* Canonoicalize filenames. Some of the paths may have redundant `.`s or `..`s in
             them. Although canonicalizing is not necessary for correctness, it makes the
             output a bit nicer. *)
          let file =
            Misc.canonicalize_filename ?cwd:config.merlin.source_root
              loc.loc_start.pos_fname
          in
          let loc = set_fname ~file loc in
          let lid = try Longident.head txt with _ -> "not flat lid" in
          log ~title:"occurrences" "Found occ: %s %a" lid Logger.fmt
            (Fun.flip Location.print_loc loc);
          (* Merlin-jst: See comment at the commented-out definition of last_loc for
             explanation of why this is commented out. *)
          (* let loc = last_loc loc txt in *)
          let fname = loc.Location.loc_start.Lexing.pos_fname in
          let loc =
            if not (Filename.is_relative fname) then Some loc
            else
              match config.merlin.source_root with
              | Some path ->
                let file = Filename.concat path loc.loc_start.pos_fname in
                Some (set_fname ~file loc)
              | None -> begin
                match Locate.find_source ~config loc fname with
                | `Found (file, _) -> Some (set_fname ~file loc)
                | `File_not_found msg ->
                  log ~title:"occurrences" "%s" msg;
                  None
              end
          in
          Option.map loc ~f:(fun loc : Query_protocol.occurrence ->
              { loc; is_stale = Staleness.is_stale staleness }))
    in
    let def_uid_is_in_current_unit =
      let uid_comp_unit = comp_unit_of_uid def_uid in
      Option.value_map ~default:false uid_comp_unit
        ~f:Misc_utils.is_current_unit
    in
    let status =
      match (scope, String.Set.to_list out_of_sync_files) with
      | `Project, [] -> `Included
      | `Project, l -> `Out_of_sync l
      | `Buffer, _ -> `Not_requested
    in
    if not def_uid_is_in_current_unit then { occurrences; status }
    else
      let definition_occurrence : Query_protocol.occurrence =
        { loc = set_fname ~file:current_buffer_path def_loc; is_stale = false }
      in
      let occurrences = definition_occurrence :: occurrences in
      { occurrences; status }
  | None -> { occurrences = []; status = `No_def }
