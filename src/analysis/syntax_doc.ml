open Browse_raw
open Std

type syntax_info = Query_protocol.Syntax_doc_result.t option

module Doc_website_base = struct
  type t = Ocaml | Oxcaml
end

let syntax_doc_url (doc_website_base : Doc_website_base.t) endpoint =
  let base_url =
    match doc_website_base with
    | Ocaml -> "https://v2.ocaml.org/releases/4.14/htmlman/"
    | Oxcaml -> "https://oxcaml.org/documentation/"
  in
  base_url ^ endpoint

(** Drop elements from the head of [list] until [f] returns [true]. *)
let rec drop_until list ~f =
  match list with
  | [] -> []
  | hd :: rest -> (
    match f hd with
    | true -> list
    | false -> drop_until rest ~f)

module Loc_comparison_result = struct
  type t = Before | Inside | After

  let is_inside = function
    | Before | After -> false
    | Inside -> true
end

let get_jkind_abbrev_doc abbrev =
  let open Option.Infix in
  let open struct
    type docpage = Kind_syntax | Unboxed_types
  end in
  let* description, docpage =
    match abbrev with
    | "any" ->
      Some
        ("The top of the kind lattice; all types have this kind.", Kind_syntax)
    | "any_non_null" -> Some ("A synonym for `any mod non_null`.", Kind_syntax)
    | "value_or_null" ->
      Some
        ( "The kind of ordinary OCaml types, but with the possibility that the \
           type contains `null`.",
          Kind_syntax )
    | "value" -> Some ("The kind of ordinary OCaml types", Kind_syntax)
    | "void" ->
      Some
        ( "The layout of types that are represented by 0 bits at runtime; \
           these types can contain only 1 value.",
          Kind_syntax )
    | "immediate64" ->
      Some
        ( "On 64-bit platforms, the kind of types inhabited only by tagged \
           integers.",
          Kind_syntax )
    | "immediate" ->
      Some ("The kind of types inhabited only by tagged integers.", Kind_syntax)
    | "immediate_or_null" ->
      Some
        ( "The kind of types inhabited by tagged integers and the bit pattern \
           containing all 0s.",
          Kind_syntax )
    | "float64" ->
      Some
        ( "The layout of types represented by a 64-bit machine float.",
          Unboxed_types )
    | "float32" ->
      Some
        ( "The layout of types represented by a 32-bit machine float.",
          Unboxed_types )
    | "word" ->
      Some
        ( "The layout of types represented by a native-width machine word.",
          Unboxed_types )
    | "bits8" ->
      Some
        ( "The layout of types represented by an 8-bit machine word.",
          Unboxed_types )
    | "bits16" ->
      Some
        ( "The layout of types represented by a 16-bit machine word.",
          Unboxed_types )
    | "bits32" ->
      Some
        ( "The layout of types represented by a 32-bit machine word.",
          Unboxed_types )
    | "bits64" ->
      Some
        ( "The layout of types represented by a 64-bit machine word.",
          Unboxed_types )
    | "vec128" ->
      Some
        ( "The layout of types represented by a 128-bit machine vector.",
          Unboxed_types )
    | "vec256" ->
      Some
        ( "The layout of types represented by a 256-bit machine vector.",
          Unboxed_types )
    | "vec512" ->
      Some
        ( "The layout of types represented by a 512-bit machine vector.",
          Unboxed_types )
    | "immutable_data" ->
      Some
        ( "The kind of types that contain no mutable parts and no functions.",
          Kind_syntax )
    | "sync_data" ->
      Some
        ( "The kind of types that contain no mutable parts (except possibly \
           for atomic fields) and no functions.",
          Kind_syntax )
    | "mutable_data" ->
      Some
        ( "The kind of types that may have mutable parts but contain no \
           functions.",
          Kind_syntax )
    | _ -> None
  in
  let docpage_str =
    match docpage with
    | Kind_syntax -> "kinds/syntax/"
    | Unboxed_types -> "unboxed-types/intro/"
  in
  (Some
     { name = "Kind abbreviation";
       description;
       documentation = syntax_doc_url Oxcaml docpage_str;
       level = Advanced
     }
    : syntax_info)

let get_oxcaml_syntax_doc cursor_loc nodes : syntax_info =
  (* Merlin-jst specific: This function gets documentation for oxcaml language
     extensions. *)
  let syntax_doc_url = syntax_doc_url Oxcaml in
  let compare_cursor_to_loc loc : Loc_comparison_result.t =
    match Location_aux.compare_pos cursor_loc loc with
    | n when n < 0 -> Before
    | n when n > 0 -> After
    | _ -> Inside
  in
  let nodes = List.map nodes ~f:snd in
  let nodes =
    (* Sometimes the bottom node of [nodes] doesn't include the location of the cursor.
       This seems to be because Merlin will find the bottom-most node that contains the
       cursor, but then select a child of that node via some heuristics. This is in order
       to try to find a node with the environment the user most likely wanted if they,
       say, have their cursor on a keyword that isn't represented by a node type in
       [Browse_raw.t] (see docstring on [Mtyper.node_at] for more info). But here we
       actually want the cursor to be included within all the nodes in [nodes] so that we
       can more easily reason about [nodes]. So we drop nodes from the head of [nodes]
       until we reach one that includes the cursor. *)
    drop_until nodes ~f:(fun node ->
        let loc = Browse_raw.node_merlin_loc Location.none node in
        match compare_cursor_to_loc loc with
        | Inside -> true
        | Before | After -> false)
  in
  let get_doc_for_attribute (attribute : Parsetree.attribute) : syntax_info =
    (* See below usage of this function for explanation of why this isn't part of the
       other big match statement. *)
    match attribute with
    (* Zero-alloc annotations *)
    | { attr_name = { txt = "zero_alloc"; _ }; attr_payload; _ } -> (
      match attr_payload with
      | PStr [] ->
        Some
          { name = "Zero-alloc annotation";
            description = "todo";
            documentation = syntax_doc_url "todo";
            level = Advanced
          }
      | PStr
          [ { pstr_desc =
                Pstr_eval
                  ( { pexp_desc =
                        ( Pexp_ident { txt = Lident zero_alloc_flag_name; _ }
                        | Pexp_apply
                            ( { pexp_desc =
                                  Pexp_ident
                                    { txt = Lident zero_alloc_flag_name; _ };
                                _
                              },
                              _ ) );
                      _
                    },
                    _ );
              _
            }
          ] -> (
        match zero_alloc_flag_name with
        | "opt" ->
          Some
            { name = "Zero-alloc opt annotation";
              description = "todo";
              documentation = syntax_doc_url "todo";
              level = Advanced
            }
        | "assume" ->
          Some
            { name = "Zero-alloc assume annotation";
              description = "todo";
              documentation = syntax_doc_url "todo";
              level = Advanced
            }
        | "strict" ->
          Some
            { name = "Zero-alloc strict annotation";
              description = "todo";
              documentation = syntax_doc_url "todo";
              level = Advanced
            }
        | "arity" ->
          Some
            { name = "Zero-alloc arity annotation";
              description = "todo";
              documentation = syntax_doc_url "todo";
              level = Advanced
            }
        | _ -> None)
      | _ ->
        Some
          { name = "Unrecognized zero-alloc annotation";
            description = "todo";
            documentation = syntax_doc_url "todo";
            level = Advanced
          })
    | { attr_name = { txt = "noalloc"; _ }; _ } ->
      Some
        { name = "noalloc annotation";
          description = "todo";
          documentation = syntax_doc_url "todo";
          level = Advanced
        }
    (* Inlining annotations *)
    | { attr_name = { txt = "inline"; _ }; attr_payload; _ } -> (
      match attr_payload with
      | PStr [] ->
        Some
          { name = "Inline annotation";
            description = "todo";
            documentation = syntax_doc_url "todo";
            level = Advanced
          }
      | PStr
          [ { pstr_desc =
                Pstr_eval
                  ( { pexp_desc = Pexp_ident { txt = Lident inline_flag_name; _ };
                      _
                    },
                    _ );
              _
            }
          ] -> (
        match inline_flag_name with
        | "always" ->
          Some
            { name = "Inline always annotation";
              description = "todo";
              documentation = syntax_doc_url "todo";
              level = Advanced
            }
        | "never" ->
          Some
            { name = "Inline never annotation";
              description = "todo";
              documentation = syntax_doc_url "todo";
              level = Advanced
            }
        | _ -> None)
      | _ ->
        Some
          { name = "Unrecognized inline annotation";
            description = "todo";
            documentation = syntax_doc_url "todo";
            level = Advanced
          })
    | { attr_name = { txt = "inlined"; _ }; attr_payload; _ } -> (
      match attr_payload with
      | PStr [] ->
        Some
          { name = "Inlined annotation";
            description = "todo";
            documentation = syntax_doc_url "todo";
            level = Advanced
          }
      | PStr
          [ { pstr_desc =
                Pstr_eval
                  ( { pexp_desc = Pexp_ident { txt = Lident inline_flag_name; _ };
                      _
                    },
                    _ );
              _
            }
          ] -> (
        match inline_flag_name with
        | "always" ->
          Some
            { name = "Inlined always annotation";
              description = "todo";
              documentation = syntax_doc_url "todo";
              level = Advanced
            }
        | "never" ->
          Some
            { name = "Inlined never annotation";
              description = "todo";
              documentation = syntax_doc_url "todo";
              level = Advanced
            }
        | _ -> None)
      | _ ->
        Some
          { name = "Unrecognized inlined annotation";
            description = "todo";
            documentation = syntax_doc_url "todo";
            level = Advanced
          })
    | { attr_name = { txt = "specialise"; _ }; attr_payload; _ } -> (
      match attr_payload with
      | PStr [] ->
        Some
          { name = "Specialise annotation";
            description = "todo";
            documentation = syntax_doc_url "todo";
            level = Advanced
          }
      | PStr
          [ { pstr_desc =
                Pstr_eval
                  ( { pexp_desc =
                        Pexp_ident { txt = Lident specialise_flag_name; _ };
                      _
                    },
                    _ );
              _
            }
          ] -> (
        match specialise_flag_name with
        | "always" ->
          Some
            { name = "Specialise always annotation";
              description = "todo";
              documentation = syntax_doc_url "todo";
              level = Advanced
            }
        | "never" ->
          Some
            { name = "Specialise never annotation";
              description = "todo";
              documentation = syntax_doc_url "todo";
              level = Advanced
            }
        | _ -> None)
      | _ ->
        Some
          { name = "Unrecognized specialise annotation";
            description = "todo";
            documentation = syntax_doc_url "todo";
            level = Advanced
          })
    | { attr_name = { txt = "specialised"; _ }; attr_payload; _ } -> (
      match attr_payload with
      | PStr [] ->
        Some
          { name = "Specialised annotation";
            description = "todo";
            documentation = syntax_doc_url "todo";
            level = Advanced
          }
      | PStr
          [ { pstr_desc =
                Pstr_eval
                  ( { pexp_desc =
                        Pexp_ident { txt = Lident specialise_flag_name; _ };
                      _
                    },
                    _ );
              _
            }
          ] -> (
        match specialise_flag_name with
        | "always" ->
          Some
            { name = "Specialised always annotation";
              description = "todo";
              documentation = syntax_doc_url "todo";
              level = Advanced
            }
        | "never" ->
          Some
            { name = "Specialised never annotation";
              description = "todo";
              documentation = syntax_doc_url "todo";
              level = Advanced
            }
        | _ -> None)
      | _ ->
        Some
          { name = "Unrecognized specialised annotation";
            description = "todo";
            documentation = syntax_doc_url "todo";
            level = Advanced
          })
    | { attr_name = { txt = "unrolled"; _ }; _ } ->
      Some
        { name = "unrolled annotation";
          description = "todo";
          documentation = syntax_doc_url "todo";
          level = Advanced
        }
    (* Misc *)
    | { attr_name = { txt = "nontail"; _ }; _ } ->
      Some
        { name = "nontail annotation";
          description = "todo";
          documentation = syntax_doc_url "todo";
          level = Advanced
        }
    | _ -> None
  in
  match nodes with
  (* Modes and modalities *)
  | Mode { txt = Mode _; _ } :: ancestors -> (
    match ancestors with
    | Jkind_annotation _ :: _ ->
      Some
        { name = "Mode (in kind)";
          description = "todo";
          documentation = syntax_doc_url "todo";
          level = Advanced
        }
    | _ ->
      Some
        { name = "Mode";
          description = "todo";
          documentation = syntax_doc_url "todo";
          level = Advanced
        })
  | Modality { txt = Modality _; _ } :: ancestors -> (
    match ancestors with
    | Jkind_annotation _ :: _ ->
      Some
        { name = "Modality (in kind)";
          description = "todo";
          documentation = syntax_doc_url "todo";
          level = Advanced
        }
    | _ ->
      Some
        { name = "Modality";
          description = "todo";
          documentation = syntax_doc_url "todo";
          level = Advanced
        })
  (* Jkinds *)
  | Jkind_annotation { pjkind_desc = Abbreviation abbrev; _ } :: _ ->
    get_jkind_abbrev_doc abbrev
  | Jkind_annotation { pjkind_desc = Mod _; _ } :: _ ->
    Some
      { name = "mod keyword";
        description = "todo";
        documentation = syntax_doc_url "todo";
        level = Advanced
      }
  | Jkind_annotation { pjkind_desc = With (_, with_type, _); _ } :: _ -> (
    match compare_cursor_to_loc with_type.ptyp_loc with
    | Before ->
      Some
        { name = "with keyword (kinds)";
          description = "todo";
          documentation = syntax_doc_url "todo";
          level = Advanced
        }
    | Inside ->
      Some
        { name = "with-type";
          description = "todo";
          documentation = syntax_doc_url "todo";
          level = Advanced
        }
    | After ->
      Some
        { name = "@@ keyword";
          description = "todo";
          documentation = syntax_doc_url "todo";
          level = Advanced
        })
  (* Module Strengthening *)
  | Module_type { mty_desc = Tmty_strengthen (_, _, mod_ident); _ } :: _ -> (
    (* Due to a current bug, there is no node for the module name after the `with`, so
       it's possible the cursor is on that instead of the `with`. *)
    match compare_cursor_to_loc mod_ident.loc with
    | Before ->
      Some
        { name = "with keyword (module strengthening)";
          description = "todo";
          documentation = syntax_doc_url "todo";
          level = Advanced
        }
    | Inside | After -> None)
  (* Local allocations *)
  | Expression { exp_desc = Texp_exclave _; _ } :: _ ->
    Some
      { name = "exclave_";
        description = "todo";
        documentation = syntax_doc_url "todo";
        level = Advanced
      }
  | Expression { exp_extra; exp_loc; _ } :: _
    when List.exists exp_extra ~f:(fun (extra, _, _) ->
             match extra with
             | Typedtree.Texp_stack -> true
             | _ -> false)
         && (* In this case, [exp_loc] differs from the location returned by
               [Browse_raw.node_merlin_loc] (which is whats used to determine [nodes]).
               The [Browse_raw.node_merlin_loc] one includes the stack_, whereas [exp_loc]
               doesn't. Since we already know that the cursor is in the
               [Browse_raw.node_merlin_loc] location (see the usage of [drop_until]
               above), we just need to check whether its in [exp_loc] to know whether it's
               on the [stack_] keyword. *)
         not (Loc_comparison_result.is_inside (compare_cursor_to_loc exp_loc))
    ->
    Some
      { name = "stack_";
        description = "todo";
        documentation = syntax_doc_url "todo";
        level = Advanced
      }
  (* Include functor *)
  | ( Include_description
        { incl_kind = Tincl_functor _ | Tincl_gen_functor _; _ }
    | Include_declaration
        { incl_kind = Tincl_functor _ | Tincl_gen_functor _; _ } )
    :: _ ->
    Some
      { name = "include functor";
        description = "todo";
        documentation = syntax_doc_url "todo";
        level = Advanced
      }
  | nodes ->
    (* The locations of attributes nodes only include the attribute name, not the payload.
       Additionally, the attribute node is not a parent of the payload node. But the
       attribute node will be a sibling of the payload. (Note that the bottom node might
       not be the payload but a node within the payload). So here we walk up the list of
       ancestors until we find one with an attribute as a child whose location includes
       the cursor position, at which point we can conclude the cursor is in the payload. *)
    List.find_map_opt nodes ~f:(fun ancestor ->
        let children =
          Browse_raw.fold_node
            (fun _ child acc -> child :: acc)
            Env.empty ancestor []
        in
        List.find_map_opt children ~f:(fun child ->
            match child with
            | Attribute attribute -> (
              match compare_cursor_to_loc attribute.attr_loc with
              | Inside -> get_doc_for_attribute attribute
              | Before | After -> None)
            | _ -> None))

let get_syntax_doc cursor_loc node : syntax_info =
  let syntax_doc_url = syntax_doc_url Ocaml in
  match node with
  | (_, Type_kind _)
    :: (_, Type_declaration _)
    :: (_, With_constraint (Twith_typesubst _))
    :: _ ->
    Some
      { name = "Destructive substitution";
        description =
          "Behaves like normal signature constraints but removes the redefined \
           type or module from the signature.";
        documentation =
          syntax_doc_url
            "signaturesubstitution.html#ss:destructive-substitution";
        level = Simple
      }
  | (_, Type_kind _)
    :: (_, Type_declaration _)
    :: (_, Signature_item ({ sig_desc = Tsig_typesubst _; _ }, _))
    :: _ ->
    Some
      { name = "Local substitution";
        description =
          "Behaves like destructive substitution but is introduced during the \
           specification of the signature, and will apply to all the items \
           that follow.";
        documentation =
          syntax_doc_url "signaturesubstitution.html#ss:local-substitution";
        level = Simple
      }
  | (_, Module_type _)
    :: (_, Module_type _)
    :: ( _,
         Module_type_constraint
           (Tmodtype_explicit
             { mty_desc = Tmty_with (_, [ (_, _, Twith_modtype _) ]); _ }) )
    :: _ ->
    Some
      { name = "Module substitution";
        description =
          "Behaves like type substitutions but are useful to refine an \
           abstract module type in a signature into a concrete module type,";
        documentation =
          syntax_doc_url
            "signaturesubstitution.html#ss:module-type-substitution";
        level = Simple
      }
  | (_, Type_kind Ttype_open) :: (_, Type_declaration { typ_private; _ }) :: _
    ->
    let e_name = "Extensible Variant Type" in
    let e_description =
      "Can be extended with new variant constructors using `+=`."
    in
    let e_url = "extensiblevariants.html" in
    let name, description, url =
      match typ_private with
      | Public -> (e_name, e_description, e_url)
      | Private ->
        ( Format.sprintf "Private %s" e_name,
          Format.sprintf
            "%s. Prevents new constructors from being declared directly, but \
             allows extension constructors to be referred to in interfaces."
            e_description,
          "extensiblevariants.html#ss:private-extensible" )
    in
    Some
      { name;
        description;
        documentation = syntax_doc_url url;
        level = Advanced
      }
  | (_, Constructor_declaration _)
    :: (_, Type_kind (Ttype_variant _))
    :: (_, Type_declaration { typ_private; _ })
    :: _
  | _
    :: (_, Constructor_declaration _)
    :: (_, Type_kind (Ttype_variant _))
    :: (_, Type_declaration { typ_private; _ })
    :: _ ->
    let v_name = "Variant Type" in
    let v_description =
      "Represent's data that may take on multiple different forms."
    in
    let v_url = "typedecl.html#ss:typedefs" in
    let name, description, url =
      match typ_private with
      | Public -> (v_name, v_description, v_url)
      | Private ->
        ( Format.sprintf "Private %s" v_name,
          Format.sprintf
            "%s This type is private, values cannot be constructed directly \
             but can be de-structured as usual."
            v_description,
          "privatetypes.html#ss:private-types-variant" )
    in
    Some
      { name; description; documentation = syntax_doc_url url; level = Simple }
  | (_, Core_type _)
    :: (_, Core_type _)
    :: (_, Label_declaration _)
    :: (_, Type_kind (Ttype_record _))
    :: (_, Type_declaration { typ_private; _ })
    :: _ ->
    let r_name = "Record Type" in
    let r_description = "Defines variants with a fixed set of fields" in
    let r_url = "typedecl.html#ss:typedefs" in
    let name, description, url =
      match typ_private with
      | Public -> (r_name, r_description, r_url)
      | Private ->
        ( Format.sprintf "Private %s" r_name,
          Format.sprintf
            "%s This type is private, values cannot be constructed directly \
             but can be de-structured as usual."
            r_description,
          "privatetypes.html#ss:private-types-variant" )
    in
    Some
      { name; description; documentation = syntax_doc_url url; level = Simple }
  | (_, Type_kind (Ttype_variant _))
    :: (_, Type_declaration { typ_private = Public; _ })
    :: _ ->
    Some
      { name = "Empty Variant Type";
        description = "An empty variant type.";
        documentation = syntax_doc_url "emptyvariants.html";
        level = Advanced
      }
  | (_, Type_kind Ttype_abstract)
    :: (_, Type_declaration { typ_private = Public; typ_manifest = None; _ })
    :: _ ->
    Some
      { name = "Abstract Type";
        description =
          "Define variants with arbitrary data structures, including other \
           variants, records, and functions";
        documentation = syntax_doc_url "typedecl.html#ss:typedefs";
        level = Simple
      }
  | (_, Type_kind Ttype_abstract)
    :: (_, Type_declaration { typ_private = Private; _ })
    :: _ ->
    Some
      { name = "Private Type Abbreviation";
        description =
          "Declares a type that is distinct from its implementation type \
           `typexpr`.";
        documentation =
          syntax_doc_url "privatetypes.html#ss:private-types-abbrev";
        level = Simple
      }
  | (_, Expression _)
    :: (_, Expression _)
    :: (_, Value_binding _)
    :: (_, Structure_item ({ str_desc = Tstr_value (Recursive, _); _ }, _))
    :: _ ->
    Some
      { name = "Recursive value definition";
        description =
          "Supports a certain class of recursive definitions of non-functional \
           values.";
        documentation = syntax_doc_url "letrecvalues.html";
        level = Simple
      }
  | (_, Module_expr _) :: (_, Module_type { mty_desc = Tmty_typeof _; _ }) :: _
    ->
    Some
      { name = "Recovering module type";
        description =
          "Expands to the module type (signature or functor type) inferred for \
           the module expression `module-expr`. ";
        documentation = syntax_doc_url "moduletypeof.html";
        level = Simple
      }
  | (_, Module_expr _)
    :: (_, Module_expr _)
    :: (_, Module_binding _)
    :: (_, Structure_item ({ str_desc = Tstr_recmodule _; _ }, _))
    :: _ ->
    Some
      { name = "Recursive module";
        description =
          "A simultaneous definition of modules that can refer recursively to \
           each others.";
        documentation = syntax_doc_url "recursivemodules.html";
        level = Simple
      }
  | (_, Expression _)
    :: (_, Expression _)
    :: (_, Expression _)
    :: ( _,
         Value_binding
           { vb_expr =
               { exp_extra = [ (Texp_newtype (_, loc, _, _), _, _) ];
                 exp_loc;
                 _
               };
             _
           } )
    :: _ -> (
    let in_range =
      cursor_loc.Lexing.pos_cnum - 1 > exp_loc.loc_start.pos_cnum
      && cursor_loc.Lexing.pos_cnum <= loc.loc.loc_end.pos_cnum + 1
    in
    match in_range with
    | true ->
      Some
        { name = "Locally Abstract Type";
          description =
            "Type constructor which is considered abstract in the scope of the \
             sub-expression and replaced by a fresh type variable.";
          documentation = syntax_doc_url "locallyabstract.html";
          level = Simple
        }
    | false -> None)
  | (_, Module_expr _)
    :: (_, Module_expr _)
    :: (_, Expression { exp_desc = Texp_pack _; _ })
    :: _ ->
    Some
      { name = "First class module";
        description =
          "Converts a module (structure or functor) to a value of the core \
           language that encapsulates the module.";
        documentation = syntax_doc_url "firstclassmodules.html";
        level = Simple
      }
  | _ -> get_oxcaml_syntax_doc cursor_loc node
