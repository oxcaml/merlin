(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Xavier Leroy, projet Gallium, INRIA Rocquencourt                     *)
(*   Gabriel Scherer, projet Parsifal, INRIA Saclay                       *)
(*                                                                        *)
(*   Copyright 2019 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Persistent structure descriptions *)

open Misc
open Cmi_format

module CU = Compilation_unit
module Consistbl_data = Import_info.Intf.Nonalias.Kind
module Consistbl = Consistbl.Make (CU.Name) (Consistbl_data)

let add_delayed_check_forward = ref (fun _ -> assert false)

type error =
  | Illegal_renaming of CU.Name.t * CU.Name.t * filepath
  | Inconsistent_import of CU.Name.t * filepath * filepath
  | Need_recursive_types of CU.Name.t
  | Inconsistent_package_declaration_between_imports of
      filepath * CU.t * CU.t
  | Direct_reference_from_wrong_package of
      CU.t * filepath * CU.Prefix.t
  | Illegal_import_of_parameter of Global_module.Name.t * filepath
  | Not_compiled_as_parameter of Global_module.Name.t
  | Imported_module_has_unset_parameter of
      { imported : Global_module.Name.t;
        parameter : Global_module.Parameter_name.t;
      }
  | Imported_module_has_no_such_parameter of
      { imported : CU.Name.t;
        valid_parameters : Global_module.Parameter_name.t list;
        parameter : Global_module.Parameter_name.t;
        value : Global_module.Name.t;
      }
  | Not_compiled_as_argument of
      { param : Global_module.Parameter_name.t;
        value : Global_module.Name.t;
        filename : filepath;
      }
  | Argument_type_mismatch of
      { value : Global_module.Name.t;
        filename : filepath;
        expected : Global_module.Parameter_name.t;
        actual : Global_module.Parameter_name.t;
      }
  | Unbound_module_as_argument_value of
      { instance: Global_module.Name.t;
        value: Global_module.Name.t;
      }

exception Error of error
let error err = raise (Error err)

module Persistent_signature = struct
  type t =
    { filename : string;
      cmi : Cmi_format.cmi_infos_lazy;
      visibility : Load_path.visibility }

  let load = ref (fun ~allow_hidden ~unit_name ->
    let unit_name = CU.Name.to_string unit_name in
    match Load_path.find_normalized_with_visibility (unit_name ^ ".cmi") with
    | filename, visibility when allow_hidden ->
      let cmi = Cmi_cache.read filename in
      Some { filename; cmi; visibility}
    | filename, Visible ->
      let cmi = Cmi_cache.read filename in
      Some { filename; cmi; visibility = Visible}
    | _, Hidden
    | exception Not_found -> None)
end

type can_load_cmis =
  | Can_load_cmis
  | Cannot_load_cmis of Lazy_backtrack.log

(* Whether a global name was first encountered in this module or by importing
   from somewhere else *)
type global_name_mentioned_by =
  | Current
  | Other of Global_module.Name.t

type global_name_info = {
  gn_global : Global_module.With_precision.t;
  gn_mentioned_by : global_name_mentioned_by; (* For error reporting *)
}

(* Data relating directly to a .cmi - does not depend on arguments *)
type import = {
  imp_is_param : bool;
  imp_params : Global_module.Parameter_name.t list;
  imp_arg_for : Global_module.Parameter_name.t option;
  imp_impl : CU.t option; (* None iff import is a parameter *)
  imp_raw_sign : Signature_with_global_bindings.t;
  imp_filename : string;
  imp_uid : Shape.Uid.t;
  imp_visibility: Load_path.visibility;
  imp_crcs : Import_info.Intf.t array;
  imp_flags : Cmi_format.pers_flags list;
}

(* If a .cmi file is missing (or invalid), we
   store it as Missing in the cache. *)
type import_info =
  | Missing
  | Found of import

(* Data relating to a global name (possibly with arguments) but not necessarily
   a value in scope. For example, if we've encountered a module only by seeing
   it used as the name or value of an argument in a [Global_module.Name.t], we
   won't bind it or construct a [pers_struct] for it but it will have a
   [pers_name]. *)
type pers_name = {
  pn_import : import;
  pn_global : Global_module.t;
  pn_sign : Subst.Lazy.signature;
}

(* What a global identifier is actually bound to in Lambda code *)
type binding =
  | Runtime_parameter of Ident.t (* Bound to a runtime parameter *)
  | Constant of Compilation_unit.t (* Bound to a static constant *)

(* Data relating to an actual referenceable module, with a signature and a
   representation in memory. *)
type 'a pers_struct_info = {
  ps_name_info: pers_name;
  ps_binding: binding;
  ps_canonical : bool;
  ps_val : 'a;
}

module Param_set = Global_module.Parameter_name.Set

(* If you add something here, _do not forget_ to add it to [clear]! *)
type 'a t = {
  globals : (Global_module.Name.t, global_name_info) Hashtbl.t;
  imports : (CU.Name.t, import_info) Hashtbl.t;
  persistent_names : (Global_module.Name.t, pers_name) Hashtbl.t;
  persistent_structures :
    (Global_module.Name.t, 'a pers_struct_info) Hashtbl.t;
  locals_bound_to_runtime_parameters : unit Ident.Tbl.t;
  imported_units: CU.Name.Set.t ref;
  imported_opaque_units: CU.Name.Set.t ref;
  param_imports : Param_set.t ref;
  crc_units: Consistbl.t;
  can_load_cmis: can_load_cmis ref;
  short_paths_basis: Short_paths.Basis.t ref;
}

let empty () = {
  globals = Hashtbl.create 17;
  imports = Hashtbl.create 17;
  persistent_names = Hashtbl.create 17;
  persistent_structures = Hashtbl.create 17;
  locals_bound_to_runtime_parameters = Ident.Tbl.create 17;
  imported_units = ref CU.Name.Set.empty;
  imported_opaque_units = ref CU.Name.Set.empty;
  param_imports = ref Param_set.empty;
  crc_units = Consistbl.create ();
  can_load_cmis = ref Can_load_cmis;
  short_paths_basis = ref (Short_paths.Basis.create ());
}

let clear penv =
  let {
    globals;
    imports;
    persistent_names;
    persistent_structures;
    locals_bound_to_runtime_parameters;
    imported_units;
    imported_opaque_units;
    param_imports;
    crc_units;
    can_load_cmis;
    short_paths_basis;
  } = penv in
  Hashtbl.clear globals;
  Hashtbl.clear imports;
  Hashtbl.clear persistent_names;
  Hashtbl.clear persistent_structures;
  Ident.Tbl.clear locals_bound_to_runtime_parameters;
  imported_units := CU.Name.Set.empty;
  imported_opaque_units := CU.Name.Set.empty;
  param_imports := Param_set.empty;
  Consistbl.clear crc_units;
  can_load_cmis := Can_load_cmis;
  short_paths_basis := Short_paths.Basis.create ();
  ()

let clear_missing {imports; _} =
  let missing_entries =
    Hashtbl.fold
      (fun name r acc -> if r = Missing then name :: acc else acc)
      imports []
  in
  List.iter (Hashtbl.remove imports) missing_entries

let add_import {imported_units; _} s =
  imported_units := CU.Name.Set.add s !imported_units

let rec add_imports_in_name penv (g : Global_module.Name.t) =
  add_import penv (g |> CU.Name.of_head_of_global_name);
  let add_in_arg ({ param; value } : Global_module.Name.argument) =
    add_import penv (param |> CU.Name.of_parameter_name);
    add_imports_in_name penv value
  in
  List.iter add_in_arg g.args

let register_import_as_opaque {imported_opaque_units; _} s =
  imported_opaque_units := CU.Name.Set.add s !imported_opaque_units

let find_import_info_in_cache {imports; _} import =
  match Hashtbl.find imports import with
  | exception Not_found -> None
  | Missing -> None
  | Found imp -> Some imp

let find_name_info_in_cache {persistent_names; _} name =
  match Hashtbl.find persistent_names name with
  | exception Not_found -> None
  | pn -> Some pn

let find_info_in_cache {persistent_structures; _} name =
  match Hashtbl.find persistent_structures name with
  | exception Not_found -> None
  | ps -> Some ps

let find_in_cache penv name =
  find_info_in_cache penv name |> Option.map (fun ps -> ps.ps_val)

let register_parameter ({param_imports; _} as penv) modname =
  let import = CU.Name.of_parameter_name modname in
  begin match find_import_info_in_cache penv import with
  | None ->
      (* Not loaded yet; if it's wrong, we'll get an error at load time *)
      ()
  | Some imp ->
      if not imp.imp_is_param then
        raise (Error (Not_compiled_as_parameter
                        (Global_module.Name.of_parameter_name modname)))
  end;
  param_imports := Param_set.add modname !param_imports

let import_crcs penv ~source crcs =
  let {crc_units; _} = penv in
  let import_crc import_info =
    let name = Import_info.Intf.name import_info in
    let info = Import_info.Intf.info import_info in
    match info with
    | None -> ()
    | Some (kind, crc) ->
        add_import penv name;
        Consistbl.check crc_units name kind crc source
  in Array.iter import_crc crcs

let check_consistency penv imp =
  try import_crcs penv ~source:imp.imp_filename imp.imp_crcs
  with Consistbl.Inconsistency {
      unit_name = name;
      inconsistent_source = source;
      original_source = auth;
      inconsistent_data = source_kind;
      original_data = auth_kind;
    } ->
    match source_kind, auth_kind with
    | Normal source_unit, Normal auth_unit
      when not (CU.equal source_unit auth_unit) ->
        error (Inconsistent_package_declaration_between_imports(
            imp.imp_filename, auth_unit, source_unit))
    | (Normal _ | Parameter), _ ->
      error (Inconsistent_import(name, auth, source))

let is_registered_parameter_import {param_imports; _} name =
  Global_module.Name.mem_parameter_set name !param_imports

let is_parameter_import t modname =
  let import = CU.Name.of_head_of_global_name modname in
  match find_import_info_in_cache t import with
  | Some { imp_is_param; _ } -> imp_is_param
  | None -> Misc.fatal_errorf "is_parameter_import %a" CU.Name.print import

let can_load_cmis penv =
  !(penv.can_load_cmis)
let set_can_load_cmis penv setting =
  penv.can_load_cmis := setting
let short_paths_basis penv =
  !(penv.short_paths_basis)

let without_cmis penv f x =
  let log = Lazy_backtrack.log () in
  let res =
    Misc.(protect_refs
            [R (penv.can_load_cmis, Cannot_load_cmis log)]
            (fun () -> f x))
  in
  Lazy_backtrack.backtrack log;
  res

let fold {persistent_structures; _} f x =
  Hashtbl.fold
    (fun name ps x -> if ps.ps_canonical then f name ps.ps_val x else x)
    persistent_structures x

let register_pers_for_short_paths penv modname ps components =
  let old_style_crcs =
    ps.ps_name_info.pn_import.imp_crcs
    |> Array.to_list
    |> List.map
         (fun import ->
            let name = Import_info.name import in
            let crc = Import_info.crc import in
            name, crc)
  in
  let deps, alias_deps =
    List.fold_left
      (fun (deps, alias_deps) (name, digest) ->
         let name_as_string = Compilation_unit.Name.to_string name in
         Short_paths.Basis.add (short_paths_basis penv) name_as_string;
         match digest with
         | None -> deps, name_as_string :: alias_deps
         | Some _ -> name_as_string :: deps, alias_deps)
      ([], []) old_style_crcs
  in
  let desc =
    Short_paths.Desc.Module.(Fresh (Signature components))
  in
  let is_deprecated =
    List.exists
      (function
        | Alerts alerts ->
          String.Map.mem "deprecated" alerts ||
          String.Map.mem "ocaml.deprecated" alerts
        | _ -> false)
      ps.ps_name_info.pn_import.imp_flags
  in
  let deprecated =
    if is_deprecated then Short_paths.Desc.Deprecated
    else Short_paths.Desc.Not_deprecated
  in
  (* CR parameterized modules: this will probably break with parameterized modules *)
  let modname_as_string = Global_module.Name.to_string modname  in
  Short_paths.Basis.load (short_paths_basis penv) modname_as_string
    deps alias_deps desc ps.ps_name_info.pn_import.imp_visibility deprecated
(* Reading persistent structures from .cmi files *)

let save_import penv crc modname impl flags filename =
  let {crc_units; _} = penv in
  List.iter
    (function
        | Rectypes -> ()
        | Alerts _ -> ()
        | Opaque -> register_import_as_opaque penv modname)
    flags;
  Consistbl.check crc_units modname impl crc filename;
  add_import penv modname

(* Add an import to the hash table. Checks that we are allowed to access
   this .cmi. *)

let acknowledge_import penv ~check modname pers_sig =
  let { Persistent_signature.filename; cmi; visibility } = pers_sig in
  let found_name = cmi.cmi_name in
  let kind = cmi.cmi_kind in
  let params = cmi.cmi_params in
  let crcs = cmi.cmi_crcs in
  let flags = cmi.cmi_flags in
  let sign = Signature_with_global_bindings.read_from_cmi cmi in
  if not (CU.Name.equal modname found_name) then
    error (Illegal_renaming(modname, found_name, filename));
  List.iter
    (function
        | Rectypes ->
            if not !Clflags.recursive_types then
              error (Need_recursive_types(modname))
        | Alerts _ -> ()
        | Opaque -> register_import_as_opaque penv modname)
    flags;
  begin match kind, CU.get_current () with
  | Normal { cmi_impl = imported_unit }, Some current_unit ->
      let access_allowed =
        CU.can_access_by_name imported_unit ~accessed_by:current_unit
      in
      if not access_allowed then
        let prefix = CU.for_pack_prefix current_unit in
        error (Direct_reference_from_wrong_package (imported_unit, filename, prefix));
  | _, _ -> ()
  end;
  let is_param =
    match kind with
    | Normal _ -> false
    | Parameter -> true
  in
  let arg_for, impl =
    match kind with
    | Normal { cmi_arg_for; cmi_impl } -> cmi_arg_for, Some cmi_impl
    | Parameter -> None, None
  in
  let uid =
    (* Awkwardly, we need to make sure the uid includes the pack prefix, which
       is only stored in the [cmi_impl], which only exists for the kind
       [Normal]. (There can be no pack prefix for a parameter, so it's not like
       we're missing information, but it is awkward.) *)
    (* CR-someday lmaurer: Just store the pack prefix separately like we used
       to. Then we wouldn't need [cmi_impl] at all. *)
    match kind with
    | Normal { cmi_impl; _ } -> Shape.Uid.of_compilation_unit_id cmi_impl
    | Parameter -> Shape.Uid.of_compilation_unit_name modname
  in
  let {imports; _} = penv in
  let import =
    { imp_is_param = is_param;
      imp_params = params;
      imp_arg_for = arg_for;
      imp_impl = impl;
      imp_raw_sign = sign;
      imp_filename = filename;
      imp_uid = uid;
      imp_visibility = visibility;
      imp_crcs = crcs;
      imp_flags = flags;
    }
  in
  if check then check_consistency penv import;
  Hashtbl.add imports modname (Found import);
  import

let read_import penv ~check modname cmi =
  let filename = Unit_info.Artifact.filename cmi in
  add_import penv modname;
  let cmi = read_cmi_lazy filename in
  let pers_sig = { Persistent_signature.filename; cmi; visibility = Visible } in
  acknowledge_import penv ~check modname pers_sig

let check_visibility ~allow_hidden imp =
  if not allow_hidden && imp.imp_visibility = Load_path.Hidden then raise Not_found

let find_import ~allow_hidden penv ~check modname =
  let {imports; _} = penv in
  if CU.Name.equal modname CU.Name.predef_exn then raise Not_found;
  match Hashtbl.find imports modname with
  | Found imp -> check_visibility ~allow_hidden imp; imp
  | Missing -> raise Not_found
  | exception Not_found ->
      match can_load_cmis penv with
      | Cannot_load_cmis _ -> raise Not_found
      | Can_load_cmis ->
          let psig =
            match !Persistent_signature.load ~allow_hidden ~unit_name:modname with
            | Some psig -> psig
            | None ->
                if allow_hidden then Hashtbl.add imports modname Missing;
                raise Not_found
          in
          add_import penv modname;
          acknowledge_import penv ~check modname psig

let remember_global { globals; _ } global ~precision ~mentioned_by =
  let global_name = Global_module.to_name global in
  match Hashtbl.find globals global_name with
  | exception Not_found ->
      Hashtbl.add globals global_name
        { gn_global = (global, precision);
          gn_mentioned_by = mentioned_by;
        }
  | { gn_global = old_global;
      gn_mentioned_by = first_mentioned_by } ->
      let new_global = global, precision in
      match
        Global_module.With_precision.meet old_global new_global
      with
      | updated_global ->
          if not (old_global == updated_global) then
            Hashtbl.replace globals global_name
              { gn_global = updated_global;
                gn_mentioned_by = first_mentioned_by }
      | exception Global_module.With_precision.Inconsistent ->
          let pp_mentioned_by ppf = function
            | Current ->
                Format.fprintf ppf "this compilation unit"
            | Other modname ->
                Style.as_inline_code Global_module.Name.print ppf modname
          in
          Misc.fatal_errorf
            "@[<hov>The name %a@ was bound to %a@ by %a@ \
             but it is instead bound to %a@ by %a.@]"
            (Style.as_inline_code Global_module.Name.print) global_name
            (Style.as_inline_code Global_module.With_precision.print) old_global
            pp_mentioned_by first_mentioned_by
            (Style.as_inline_code Global_module.With_precision.print) new_global
            pp_mentioned_by mentioned_by

let rec approximate_global_by_name penv global_name =
  let { param_imports; _ } = penv in
  (* We're not looking up this global's .cmi, so we can't know its parameters
     exactly. Therefore we don't know what the hidden arguments in the
     elaborated [Global_module.t] should be. However, we know that each hidden
     argument is (a) not a visible argument and (b) a parameter of the importing
     module (subset rule). Therefore it is a sound overapproximation to take as
     a hidden argument each known parameter that isn't the name of a visible
     argument. *)
  let ({ head; args = visible_args } : Global_module.Name.t) = global_name in
  let params_not_being_passed, visible_args =
    List.fold_left_map
      (fun params ({ param; value } : _ Global_module.Argument.t) ->
         let params = Param_set.remove param params in
         let value = approximate_global_by_name penv value in
         let arg : _ Global_module.Argument.t = { param; value } in
         params, arg)
      !param_imports
      visible_args
  in
  let hidden_args =
    Param_set.elements params_not_being_passed
  in
  let global = Global_module.create_exn head visible_args ~hidden_args in
  remember_global penv global ~precision:Approximate ~mentioned_by:Current;
  global

let current_unit_is_aux name ~allow_args =
  match CU.get_current () with
  | None -> false
  | Some current ->
      match CU.to_global_name current with
      | Some { head; args } ->
          (args = [] || allow_args)
          && CU.Name.equal name (head |> CU.Name.of_string)
      | None -> false

let current_unit_is name =
  current_unit_is_aux name ~allow_args:false

let current_unit_is_instance_of name =
  current_unit_is_aux name ~allow_args:true

(* Enforce the subset rule: we can only refer to a module if that module's
   parameters are also our parameters. This assumes that all of the arguments in
   [global] have already been checked, so we only need to check [global]
   itself (in other words, we don't need to recurse).

   Formally, the subset rule for an unelaborated global (that is, a
   [Global_module.Name.t]) says that [M[P_1:A_1]...[P_n:A_n]] is accessible if,
   for each parameter [P] that [M] takes, either [P] is one of the parameters
   [P_i], or the current compilation unit also takes [P].

   This function takes an _elaborated_ global (that is, a [Global_module.t]),
   which "bakes in" crucial information: all of the instantiated module's
   parameters are accounted for, so we need only concern ourselves with the
   syntax of the global and the current compilation unit's parameters.
   Specifically, the subset rule for an elaborated global says that
   [M[P_1:A_1]...[P_n:A_n]{Q_1:B_1}...{Q_m:B_m}] is accessible if each hidden
   argument value [B_i] is a parameter of the current unit. Operationally, this
   makes sense since the hidden argument [{Q:B}] means "as the argument [Q] to
   [M], we're passing our own parameter [B] along." (Currently [B] is always
   simply [Q] again. This is likely to change with future extensions, but the
   requirement will be the same: [B] needs to be something we're taking as a
   parameter.) *)
let check_for_unset_parameters penv global =
  List.iter
    (fun ({ param = parameter; value = arg_value } : Global_module.argument) ->
       let value_name = Global_module.to_name arg_value in
       if not (is_registered_parameter_import penv value_name) then
         error (Imported_module_has_unset_parameter {
             imported = Global_module.to_name global;
             parameter;
           }))
    global.Global_module.hidden_args

let rec global_of_global_name penv ~check name ~allow_excess_args =
  let load () =
    let pn =
      find_pers_name ~allow_hidden:true penv ~check name ~allow_excess_args
    in
    pn.pn_global
  in
  match Hashtbl.find penv.globals name with
  | { gn_global = (global, Exact); _ } -> global
  | { gn_global = (_, Approximate); _ } -> load ()
  | exception Not_found -> load ()

and compute_global penv modname ~params ~check ~allow_excess_args =
  let arg_global_by_param_name =
    List.map
      (fun ({ param = name; value } : Global_module.Name.argument) ->
         match global_of_global_name penv ~check value ~allow_excess_args with
         | value -> name, value
         | exception Not_found ->
             error
               (Unbound_module_as_argument_value { instance = modname; value }))
      modname.Global_module.Name.args
  in
  let subst : Global_module.subst =
    Global_module.Parameter_name.Map.of_list arg_global_by_param_name
  in
  if check && modname.Global_module.Name.args <> [] then begin
    let compare_by_param param1 (param2, _) =
      Global_module.Parameter_name.compare param1 param2
    in
    Misc_stdlib.List.merge_iter
      ~cmp:compare_by_param
      params
      arg_global_by_param_name
      ~left_only:
        (fun _ ->
           (* Parameter with no argument: fine (subset rule will be checked by
              [check_for_unset_parameters] later) *)
           ())
      ~right_only:
        (fun (param, value) ->
            (* Argument with no parameter: fine only if allowed by flag *)
            if not allow_excess_args then
              raise
                (Error (Imported_module_has_no_such_parameter {
                          imported = CU.Name.of_head_of_global_name modname;
                          valid_parameters = params;
                          parameter = param;
                          value = value |> Global_module.to_name;
                        })))
      ~both:
        (fun expected_type (_arg_name, arg_value_global) ->
            let arg_value = arg_value_global |> Global_module.to_name in
            let pn =
              find_pers_name ~allow_hidden:true penv ~check arg_value
                ~allow_excess_args
            in
            let actual_type =
              match pn.pn_import.imp_arg_for with
              | None ->
                  error (Not_compiled_as_argument
                           { param = expected_type; value = arg_value;
                             filename = pn.pn_import.imp_filename })
              | Some ty -> ty
            in
            if not (Global_module.Parameter_name.equal expected_type actual_type)
            then begin
              raise (Error (Argument_type_mismatch {
                  value = arg_value;
                  filename = pn.pn_import.imp_filename;
                  expected = expected_type;
                  actual = actual_type;
                }))
            end)
  end;
  (* Form the name without any arguments at all, then substitute in all the
     arguments. A bit roundabout but should be sound *)
  let global_without_args =
    (* Won't raise an exception, since the hidden args are all different
       (since the params are different, or else we have bigger problems) *)
    Global_module.create_exn modname.Global_module.Name.head [] ~hidden_args:params
  in
  let global, _changed = Global_module.subst global_without_args subst in
  global

and acknowledge_pers_name penv check global_name import ~allow_excess_args =
  let {persistent_names; _} = penv in
  let params = import.imp_params in
  let global =
    compute_global penv global_name ~params ~check ~allow_excess_args
  in
  (* Check whether this global is already known. Possible if there are excess
     arguments (or there were in a previous call) since then more than one
     [global_name] will map to the same [global]. *)
  let canonical_global_name =
    (* The minimal form of the global name, without any excess arguments *)
    Global_module.to_name global
  in
  let pn =
    match Hashtbl.find_opt persistent_names canonical_global_name with
    | Some pn ->
        pn
    | None ->
        acknowledge_new_pers_name penv check canonical_global_name global import
  in
  if not (Global_module.Name.equal global_name canonical_global_name) then
    (* Just remember that both names point here. Note that we don't call
       [remember_global], since it will already have been called by
       [acknowledge_new_pers_name] (either just now or earlier). This is
       annoying in the case that there were _visible_ excess arguments, since
       the approximation will just stay in [penv.globals], but it doesn't do
       any damage and at some point it will be substituted away. *)
    (* CR-someday lmaurer: Modify [remember_global] so that it can remember
       multiple global names mapped to the same global. Only likely to be
       relevant if there are _a lot_ of bound globals. *)
    Hashtbl.add persistent_names global_name pn;
  pn
and acknowledge_new_pers_name penv check global_name global import =
  (* This checks only [global] itself without recursing into argument values.
     That's fine, however, since those argument values will have come from
     recursive calls to [global_of_global_name] and therefore have passed
     through here already. *)
  check_for_unset_parameters penv global;
  let {persistent_names; _} = penv in
  let sign = import.imp_raw_sign in
  let sign =
    let bindings =
      List.map
        (fun ({ param; value } : Global_module.argument) -> param, value)
        global.Global_module.visible_args
    in
    (* Only need to substitute the visible args, since the hidden args only
       reflect substitutions already made by visible args *)
    Signature_with_global_bindings.subst sign bindings
  in
  Array.iter
    (fun (bound_global, precision) ->
       remember_global penv bound_global ~precision
         ~mentioned_by:(Other global_name))
    sign.bound_globals;
  let pn = { pn_import = import;
             pn_global = global;
             pn_sign = sign.sign;
           } in
  if check then check_consistency penv import;
  Hashtbl.add persistent_names global_name pn;
  remember_global penv global ~precision:Exact ~mentioned_by:Current;
  pn

and find_pers_name ~allow_hidden penv ~check name ~allow_excess_args =
  let {persistent_names; _} = penv in
  match Hashtbl.find persistent_names name with
  | pn -> pn
  | exception Not_found ->
      let unit_name = CU.Name.of_head_of_global_name name in
      let import = find_import ~allow_hidden penv ~check unit_name in
      acknowledge_pers_name penv check name import ~allow_excess_args

let read_pers_name penv check name filename =
  let unit_name = CU.Name.of_head_of_global_name name in
  let import = read_import penv ~check unit_name filename in
  acknowledge_pers_name penv check name import

let normalize_global_name penv modname =
  let new_modname =
    global_of_global_name penv modname ~check:true ~allow_excess_args:true
    |> Global_module.to_name
  in
  if Global_module.Name.equal modname new_modname then modname else new_modname

let need_local_ident penv (global : Global_module.t) =
  (* There are three equivalent ways to phrase the question we're asking here:

     1. Is this either a parameter or an open import?
     2. Will the generated lambda code need a parameter to take this module's
          value?
     3. Is the value not statically bound?

     Crucially, all modules (besides the one being compiled or instantiated)
     must be either statically bound or toplevel parameters, since the actual
     functor calls that instantiate open modules happen elsewhere (so that they
     can happen exactly once). *)
  let global_name = global |> Global_module.to_name in
  let name = global_name |> CU.Name.of_head_of_global_name in
  if is_registered_parameter_import penv global_name
  then
    (* Already a parameter *)
    true
  else if current_unit_is name
  then
    (* Not actually importing it in the sense of needing its value (we're
       building its value!) *)
    false
  else if current_unit_is_instance_of name
  then
    (* We're instantiating the module, so (here and only here!) we're accessing
       its actual functor, which is a compile-time constant *)
    (* CR lmaurer: Relying on [current_unit_is_instance_of] here feels hacky
       when only a pretty specific call sequence gets here. *)
    false
  else if Global_module.is_complete global
  then
    (* It's a compile-time constant *)
    false
  else
    (* Some argument is missing, or some argument's argument is missing, etc.,
       so it's not a compile-time constant *)
    true

let make_binding penv (global : Global_module.t) (impl : CU.t option) : binding =
  let name = Global_module.to_name global in
  if need_local_ident penv global
  then Runtime_parameter (Ident.create_local_binding_for_global name)
  else
    let unit_from_cmi =
      match impl with
      | Some unit -> unit
      | None ->
          Misc.fatal_errorf
            "Can't bind a parameter statically:@ %a"
            Global_module.print global
    in
    let unit =
      match global.visible_args with
      | [] ->
          (* Make sure the names are consistent up to the pack prefix *)
          assert (Global_module.Name.equal
                    (unit_from_cmi |> CU.to_global_name_without_prefix)
                    name);
          unit_from_cmi
      | _ ->
          (* Make sure the unit isn't supposed to be packed *)
          assert (not (CU.is_packed unit_from_cmi));
          CU.of_complete_global_exn global
    in
    Constant unit

type address =
  | Aunit of Compilation_unit.t
  | Alocal of Ident.t
  | Adot of address * int

type 'a sig_reader =
  Subst.Lazy.signature
  -> Global_module.Name.t
  -> Shape.Uid.t
  -> shape:Shape.t
  -> address:address
  -> flags:Cmi_format.pers_flags list
  -> 'a

(* Add a persistent structure to the hash table and bind it in the [Env].
   Checks that OCaml source is allowed to refer to this module. *)

let acknowledge_new_pers_struct penv modname pers_name val_of_pers_sig short_path_comps =
  let {persistent_structures; locals_bound_to_runtime_parameters; _} = penv in
  let import = pers_name.pn_import in
  let global = pers_name.pn_global in
  let sign = pers_name.pn_sign in
  let is_param = import.imp_is_param in
  let impl = import.imp_impl in
  let filename = import.imp_filename in
  let uid = import.imp_uid in
  let flags = import.imp_flags in
  begin match is_param, is_registered_parameter_import penv modname with
  | true, false ->
      error (Illegal_import_of_parameter(modname, filename))
  | false, true ->
      error (Not_compiled_as_parameter modname)
  | true, true
  | false, false -> ()
  end;
  let binding = make_binding penv global impl in
  let address : address =
    match binding with
    | Runtime_parameter id -> Alocal id
    | Constant unit -> Aunit unit
  in
  let shape =
    match import.imp_impl, import.imp_params with
    | Some unit, [] -> Shape.for_persistent_unit (CU.full_path_as_string unit)
    | _, _ ->
        (* TODO Implement shapes for parameters and parameterised modules *)
        Shape.error ~uid "parameter or parameterised module"
  in
  let pm = val_of_pers_sig sign modname uid ~shape ~address ~flags in
  let ps =
    { ps_name_info = pers_name;
      ps_binding = binding;
      ps_val = pm;
      ps_canonical = true;
    }
  in
  Hashtbl.add persistent_structures modname ps;
  register_pers_for_short_paths penv modname ps (short_path_comps modname pm);
  begin match binding with
  | Runtime_parameter id -> Ident.Tbl.add locals_bound_to_runtime_parameters id ()
  | Constant _ -> ()
  end;
  ps

let acknowledge_pers_struct penv modname pers_name val_of_pers_sig short_path_comps =
  (* This is the same dance that [acknowledge_pers_name] does. See comments
     there. *)
  let {persistent_structures; _} = penv in
  let canonical_modname = Global_module.to_name pers_name.pn_global in
  let ps =
    match Hashtbl.find_opt persistent_structures canonical_modname with
    | Some ps -> ps
    | None ->
        acknowledge_new_pers_struct penv canonical_modname pers_name
          val_of_pers_sig short_path_comps
  in
  if not (Global_module.Name.equal modname canonical_modname) then
    Hashtbl.add persistent_structures modname { ps with ps_canonical = false };
  ps

let read_pers_struct penv check modname cmi =
  let pers_name =
    read_pers_name penv check modname cmi ~allow_excess_args:false
  in
  pers_name.pn_sign

let find_pers_struct
    ~allow_hidden penv val_of_pers_sig short_path_comps ~check name ~allow_excess_args =
  let {persistent_structures; _} = penv in
  match Hashtbl.find persistent_structures name with
  | ps -> check_visibility ~allow_hidden ps.ps_name_info.pn_import; ps
  | exception Not_found ->
      let pers_name =
        find_pers_name ~allow_hidden penv ~check name ~allow_excess_args
      in
      acknowledge_pers_struct penv name pers_name val_of_pers_sig short_path_comps

let describe_prefix ppf prefix =
  if CU.Prefix.is_empty prefix then
    Format.fprintf ppf "outside of any package"
  else
    Format.fprintf ppf "package %a" CU.Prefix.print prefix

module Style = Misc.Style
(* Emits a warning if there is no valid cmi for name *)
let check_pers_struct ~allow_hidden penv f1 f2 ~loc name =
  let name_as_string = CU.Name.to_string (CU.Name.of_head_of_global_name name) in
  try
    ignore (find_pers_struct ~allow_hidden penv f1 f2 ~check:false name
              ~allow_excess_args:true)
  with
  | Not_found ->
      let warn = Warnings.No_cmi_file(name_as_string, None) in
        Location.prerr_warning loc warn
  | Magic_numbers.Cmi.Error err ->
      let msg = Format.asprintf "%a" Magic_numbers.Cmi.report_error err in
      let warn = Warnings.No_cmi_file(name_as_string, Some msg) in
        Location.prerr_warning loc warn
  | Error err ->
      let msg =
        match err with
        | Illegal_renaming(name, ps_name, filename) ->
            Format.asprintf
              " %a@ contains the compiled interface for @ \
               %a when %a was expected"
              (Style.as_inline_code Location.print_filename) filename
              (Style.as_inline_code CU.Name.print) ps_name
              (Style.as_inline_code CU.Name.print) name
        | Inconsistent_import _ ->
            (* Can't be raised by [find_pers_struct ~check:false] *)
            assert false
        | Need_recursive_types name ->
            Format.asprintf
              "%a uses recursive types"
              (Style.as_inline_code CU.Name.print) name
        | Inconsistent_package_declaration_between_imports _ ->
            (* Can't be raised by [find_pers_struct ~check:false] *)
            assert false
        | Direct_reference_from_wrong_package (unit, _filename, prefix) ->
            Format.asprintf "%a is inaccessible from %a"
              CU.print unit
              describe_prefix prefix
        | Illegal_import_of_parameter (name, _) ->
            Format.asprintf "%a is a parameter"
              (Style.as_inline_code Global_module.Name.print) name
        | Not_compiled_as_parameter name ->
            Format.asprintf "%a should be a parameter but isn't"
              (Style.as_inline_code Global_module.Name.print) name
        | Imported_module_has_unset_parameter { imported; parameter } ->
            Format.asprintf "%a requires argument for %a"
              (Style.as_inline_code Global_module.Name.print) imported
              (Style.as_inline_code Global_module.Parameter_name.print)
              parameter
        | Imported_module_has_no_such_parameter { imported; parameter; _ } ->
            Format.asprintf "%a has no parameter %a"
              (Style.as_inline_code CU.Name.print) imported
              (Style.as_inline_code Global_module.Parameter_name.print)
              parameter
        | Not_compiled_as_argument { value; _ } ->
            Format.asprintf "%a is not compiled as an argument"
              (Style.as_inline_code Global_module.Name.print) value
        | Argument_type_mismatch { value; expected; actual; _ } ->
            Format.asprintf "%a implements %a, not %a"
              (Style.as_inline_code Global_module.Name.print) value
              (Style.as_inline_code Global_module.Parameter_name.print) actual
              (Style.as_inline_code Global_module.Parameter_name.print) expected
        | Unbound_module_as_argument_value { value; _ } ->
            Format.asprintf "Can't find argument %a"
              (Style.as_inline_code Global_module.Name.print) value
      in
      let warn = Warnings.No_cmi_file(name_as_string, Some msg) in
        Location.prerr_warning loc warn

let read penv modname a =
  read_pers_struct penv true modname a

let find ~allow_hidden penv f1 f2 name ~allow_excess_args =
  (find_pers_struct ~allow_hidden ~allow_excess_args penv f1 f2 ~check:true
     name).ps_val

let check ~allow_hidden penv f1 f2 ~loc name =
  let {persistent_structures; _} = penv in
  if not (Hashtbl.mem persistent_structures name) then begin
    (* PR#6843: record the weak dependency ([add_import]) regardless of
       whether the check succeeds, to help make builds more
       deterministic. *)
    add_imports_in_name penv name;
    let _ : Global_module.t =
      (* Record an overapproximation of the elaborated form of this name so that
         substitution will work when the signature we're compiling is imported
         later *)
      approximate_global_by_name penv name
    in
    if (Warnings.is_active (Warnings.No_cmi_file("", None))) then
      !add_delayed_check_forward
        (fun () -> check_pers_struct ~allow_hidden penv f1 f2 ~loc name)
  end

let crc_of_unit penv name =
  match Consistbl.find penv.crc_units name with
  | Some (_impl, crc) -> crc
  | None ->
    let import = find_import ~allow_hidden:true penv ~check:true name in
    match Array.find_opt (Import_info.Intf.has_name ~name) import.imp_crcs with
    | None -> assert false
    | Some import_info ->
      match Import_info.crc import_info with
      | None -> assert false
      | Some crc -> crc

let imports {imported_units; crc_units; _} =
  let imports =
    Consistbl.extract (CU.Name.Set.elements !imported_units)
      crc_units
  in
  List.map (fun (cu_name, spec) -> Import_info.Intf.create cu_name spec)
    imports

let is_imported_parameter penv modname =
  match find_info_in_cache penv modname with
  | Some pers_struct -> pers_struct.ps_name_info.pn_import.imp_is_param
  | None -> false

let runtime_parameter_bindings {persistent_structures; _} =
  (* This over-approximates the runtime parameters that are actually needed:
     some modules get looked at during type checking but aren't relevant to
     generated lambda code. This is increasingly true with modes and layouts: we
     might need to check [P.t]'s layout but never end up using [P] directly, in
     which case `P` will end up a runtime parameter that's not needed.

     On the other hand, extra parameters here don't necessarily hurt much: they
     make [-instantiate] work harder but inlining should eliminate the actual
     runtime performance hit. If we do end up caring, probably what we need is
     to coordinate with [Translmod] so that we're asking "what all did we access
     during lambda generation?" rather than "what all did anyone ask about
     ever?". *)
  persistent_structures
  |> Hashtbl.to_seq_values
  |> Seq.filter_map
        (fun ps ->
           match ps.ps_binding with
           | Runtime_parameter local_ident ->
               if ps.ps_canonical then
                 Some (ps.ps_name_info.pn_global, local_ident)
               else
                 (* This is a forward from a non-canonical name, not an entry we
                    need to keep *)
                 None
           | Constant _ -> None)
  |> List.of_seq

let is_bound_to_runtime_parameter {locals_bound_to_runtime_parameters; _} id =
  Ident.Tbl.mem locals_bound_to_runtime_parameters id

let parameters {param_imports; _} =
  Param_set.elements !param_imports

let looked_up {persistent_structures; _} modname =
  Hashtbl.mem persistent_structures modname

let is_imported_opaque {imported_opaque_units; _} s =
  CU.Name.Set.mem s !imported_opaque_units

let implemented_parameter penv modname =
  match find_name_info_in_cache penv modname with
  | Some { pn_import = { imp_arg_for; _ }; _ } -> imp_arg_for
  | None -> None

let make_cmi penv modname kind sign alerts =
  let flags =
    List.concat [
      if !Clflags.recursive_types then [Cmi_format.Rectypes] else [];
      if !Clflags.opaque then [Cmi_format.Opaque] else [];
      [Alerts alerts];
    ]
  in
  let params =
    (* Needs to be consistent with [Translmod] *)
    parameters penv
  in
  (* Need to calculate [params] before these since [global_of_global_name] has
     side effects *)
  let crcs = imports penv in
  let globals =
    Hashtbl.to_seq_values penv.globals
    |> Seq.filter_map
         (fun { gn_global; _ } ->
            let global, _precision = gn_global in
            if Global_module.is_complete global then
              (* The globals we need to remember here are the ones that are
                 relevant for substitution. A complete global is precisely one
                 for which no further substitutions can apply. *)
              None
            else Some gn_global)
    |> Array.of_seq
  in
  {
    cmi_name = modname;
    cmi_kind = kind;
    cmi_globals = globals;
    cmi_sign = sign;
    cmi_params = params;
    cmi_crcs = Array.of_list crcs;
    cmi_flags = flags
  }

let save_cmi penv psig =
  let { Persistent_signature.filename; cmi; _ } = psig in
  Misc.try_finally (fun () ->
      let {
        cmi_name = modname;
        cmi_kind = kind;
        cmi_flags = flags;
      } = cmi in
      let crc =
        output_to_file_via_temporary (* see MPR#7472, MPR#4991 *)
          ~mode: [Open_binary] filename
          (fun temp_filename oc -> output_cmi temp_filename oc cmi) in
      (* Enter signature in consistbl so that imports() and crc_of_unit() will
         also return its crc *)
      let data : Import_info.Intf.Nonalias.Kind.t =
        match kind with
        | Normal { cmi_impl } -> Normal cmi_impl
        | Parameter -> Parameter
      in
      save_import penv crc modname data flags filename
    )
    ~exceptionally:(fun () -> remove_file filename)

let report_error ppf =
  let open Format in
  function
  | Illegal_renaming(modname, ps_name, filename) -> fprintf ppf
      "Wrong file naming: %a@ contains the compiled interface for@ \
       %a when %a was expected"
      (Style.as_inline_code Location.print_filename) filename
      (Style.as_inline_code CU.Name.print) ps_name
      (Style.as_inline_code CU.Name.print) modname
  | Inconsistent_import(name, source1, source2) -> fprintf ppf
      "@[<hov>The files %a@ and %a@ \
              make inconsistent assumptions@ over interface %a@]"
      (Style.as_inline_code Location.print_filename) source1
      (Style.as_inline_code Location.print_filename) source2
      (Style.as_inline_code CU.Name.print) name
  | Need_recursive_types(import) ->
      fprintf ppf
        "@[<hov>Invalid import of %a, which uses recursive types.@ \
         The compilation flag %a is required@]"
        (Style.as_inline_code CU.Name.print) import
        Style.inline_code "-rectypes"
  | Inconsistent_package_declaration_between_imports (filename, unit1, unit2) ->
      fprintf ppf
        "@[<hov>The file %s@ is imported both as %a@ and as %a.@]"
        filename
        (Style.as_inline_code CU.print) unit1
        (Style.as_inline_code CU.print) unit2
  | Direct_reference_from_wrong_package(unit, filename, prefix) ->
      fprintf ppf
        "@[<hov>Invalid reference to %a (in file %s) from %a.@ %s]"
        (Style.as_inline_code CU.print) unit
        filename
        describe_prefix prefix
        "Can only access members of this library's package or a containing package"
  | Illegal_import_of_parameter(modname, filename) ->
      fprintf ppf
        "@[<hov>The file %a@ contains the interface of a parameter.@ \
         %a@ is not declared as a parameter for the current unit.@]@.\
         @[<hov>@{<hint>Hint@}: \
           @[<hov>Compile the current unit with \
           @{<inline_code>-parameter %a@}.@]@]"
        (Style.as_inline_code Location.print_filename) filename
        (Style.as_inline_code Global_module.Name.print) modname
        Global_module.Name.print modname
  | Not_compiled_as_parameter modname ->
      fprintf ppf
        "@[<hov>The module %a@ is a parameter but is not declared as such for the \
         current unit.@]@.\
         @[<hov>@{<hint>Hint@}: \
           @[<hov>Compile the current unit with @{<inline_code>-parameter \
           %a@}.@]@]"
        (Style.as_inline_code Global_module.Name.print) modname
        Global_module.Name.print modname
  | Imported_module_has_unset_parameter
        { imported = modname; parameter = param } ->
      fprintf ppf
        "@[<hov>The module %a@ is not accessible because it takes %a@ \
         as a parameter and the current unit does not.@]@.\
         @[<hov>@{<hint>Hint@}: \
           @[<hov>Pass @{<inline_code>-parameter %a@}@ to add %a@ as a parameter@ \
           of the current unit.@]@]"
        (Style.as_inline_code Global_module.Name.print) modname
        (Style.as_inline_code Global_module.Parameter_name.print) param
        Global_module.Parameter_name.print param
        (Style.as_inline_code Global_module.Parameter_name.print) param
  | Imported_module_has_no_such_parameter
        { valid_parameters; imported = modname; parameter = param; value = _; } ->
      let pp_hint ppf () =
        match valid_parameters with
        | [] ->
            fprintf ppf
              "Compile %a@ with @{<inline_code>-parameter %a@}@ to make it a \
               parameter."
              (Style.as_inline_code CU.Name.print) modname
              Global_module.Parameter_name.print param
        | _ ->
          let print_params =
            Format.pp_print_list ~pp_sep:Format.pp_print_space
              (Style.as_inline_code Global_module.Parameter_name.print)
          in
          fprintf ppf "Parameters for %a:@ @[<hov>%a@]"
            (Style.as_inline_code CU.Name.print) modname
            print_params valid_parameters
      in
      fprintf ppf
        "@[<hov>The module %a@ has no parameter %a.@]@.\
         @[<hov>@{<hint>Hint@}: @[<hov>%a@]@]"
        (Style.as_inline_code CU.Name.print) modname
        (Style.as_inline_code Global_module.Parameter_name.print) param
        pp_hint ()
  | Not_compiled_as_argument { param; value; filename } ->
      fprintf ppf
        "@[<hov>The module %a@ cannot be used as an argument for parameter \
           %a.@]@.\
         @[<hov>@{<hint>Hint@}: \
           @[<hov>Compile %a@ with @{<inline_code>-as-argument-for %a@}.@]@]"
        (Style.as_inline_code Global_module.Name.print) value
        (Style.as_inline_code Global_module.Parameter_name.print) param
        (Style.as_inline_code Location.print_filename) filename
        Global_module.Parameter_name.print param
  | Argument_type_mismatch { value; filename; expected; actual; } ->
      fprintf ppf
        "@[<hov>The module %a@ is used as an argument for the parameter %a@ \
         but %a@ is an argument for %a.@]@.\
         @[<hov>@{<hint>Hint@}: \
           @[<hov>%a@ was compiled with \
             @{<inline_code>-as-argument-for %a@}.@]@]"
        (Style.as_inline_code Global_module.Name.print) value
        (Style.as_inline_code Global_module.Parameter_name.print) expected
        (Style.as_inline_code Global_module.Name.print) value
        (Style.as_inline_code Global_module.Parameter_name.print) actual
        (Style.as_inline_code Location.print_filename) filename
        Global_module.Parameter_name.print expected
  | Unbound_module_as_argument_value { instance; value } ->
      fprintf ppf
        "@[<hov>Unbound module %a@ in instance %a@]"
        (Style.as_inline_code Global_module.Name.print) value
        (Style.as_inline_code Global_module.Name.print) instance

let () =
  Location.register_error_of_exn
    (function
      | Error err ->
          (* Note that this module don't have location info in its errors, since
             (unlike [Env]) it doesn't take [Location.t]s as arguments. However,
             [Env] is often able to add location info to our errors by
             re-raising them with the [Env.Error_from_persistent_env]
             constructor. *)
          Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )

(* helper for merlin *)

let with_cmis penv f x =
  Misc.(protect_refs
          [R (penv.can_load_cmis, Can_load_cmis)]
          (fun () -> f x))

let forall ~found ~missing t =
  Std.Hashtbl.forall t.imports (fun name -> function
      | Missing -> missing name
      | Found import ->
        found name import.imp_filename name
    )
