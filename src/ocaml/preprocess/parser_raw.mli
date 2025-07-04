
(* The type of tokens. *)

type token = 
  | WITH
  | WHILE
  | WHEN
  | VIRTUAL
  | VAL
  | UNIQUE
  | UNDERSCORE
  | UIDENT of (string)
  | TYPE
  | TRY
  | TRUE
  | TO
  | TILDE
  | THEN
  | STRUCT
  | STRING of (string * Location.t * string option)
  | STAR
  | STACK
  | SIG
  | SEMISEMI
  | SEMI
  | RPAREN
  | REC
  | RBRACKET
  | RBRACE
  | QUOTED_STRING_ITEM of (string * Location.t * string * Location.t * string option)
  | QUOTED_STRING_EXPR of (string * Location.t * string * Location.t * string option)
  | QUOTE
  | QUESTION
  | PRIVATE
  | PREFIXOP of (string)
  | PLUSEQ
  | PLUSDOT
  | PLUS
  | PERCENT
  | OVERWRITE
  | OR
  | OPTLABEL of (string)
  | OPEN
  | ONCE
  | OF
  | OBJECT
  | NONREC
  | NEW
  | MUTABLE
  | MODULE
  | MOD
  | MINUSGREATER
  | MINUSDOT
  | MINUS
  | METHOD
  | MATCH
  | LPAREN
  | LOCAL
  | LIDENT of (string)
  | LETOP of (string)
  | LET
  | LESSMINUS
  | LESS
  | LBRACKETPERCENTPERCENT
  | LBRACKETPERCENT
  | LBRACKETLESS
  | LBRACKETGREATER
  | LBRACKETCOLON
  | LBRACKETBAR
  | LBRACKETATATAT
  | LBRACKETATAT
  | LBRACKETAT
  | LBRACKET
  | LBRACELESS
  | LBRACE
  | LAZY
  | LABEL of (string)
  | KIND_OF
  | KIND_ABBREV
  | INT of (string * char option)
  | INITIALIZER
  | INHERIT
  | INFIXOP4 of (string)
  | INFIXOP3 of (string)
  | INFIXOP2 of (string)
  | INFIXOP1 of (string)
  | INFIXOP0 of (string)
  | INCLUDE
  | IN
  | IF
  | HASH_SUFFIX
  | HASH_INT of (string * char option)
  | HASH_FLOAT of (string * char option)
  | HASHOP of (string)
  | HASHLPAREN
  | HASHLBRACE
  | HASH
  | GREATERRBRACKET
  | GREATERRBRACE
  | GREATERDOT
  | GREATER
  | GLOBAL
  | FUNCTOR
  | FUNCTION
  | FUN
  | FOR
  | FLOAT of (string * char option)
  | FALSE
  | EXTERNAL
  | EXCLAVE
  | EXCEPTION
  | EQUAL
  | EOL
  | EOF
  | END
  | ELSE
  | DOWNTO
  | DOTTILDE
  | DOTOP of (string)
  | DOTLESS
  | DOTHASH
  | DOTDOT
  | DOT
  | DONE
  | DOCSTRING of (Docstrings.docstring)
  | DO
  | CONSTRAINT
  | COMMENT of (string * Location.t)
  | COMMA
  | COLONRBRACKET
  | COLONGREATER
  | COLONEQUAL
  | COLONCOLON
  | COLON
  | CLASS
  | CHAR of (char)
  | BEGIN
  | BARRBRACKET
  | BARBAR
  | BAR
  | BANG
  | BACKQUOTE
  | ATAT
  | AT
  | ASSERT
  | AS
  | ANDOP of (string)
  | AND
  | AMPERSAND
  | AMPERAMPER

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val use_file: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parsetree.toplevel_phrase list)

val toplevel_phrase: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parsetree.toplevel_phrase)

val parse_val_longident: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Longident.t)

val parse_pattern: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parsetree.pattern)

val parse_mty_longident: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Longident.t)

val parse_module_type: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parsetree.module_type)

val parse_module_expr: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parsetree.module_expr)

val parse_mod_longident: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Longident.t)

val parse_mod_ext_longident: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Longident.t)

val parse_expression: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parsetree.expression)

val parse_core_type: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parsetree.core_type)

val parse_constr_longident: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Longident.t)

val parse_any_longident: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Longident.t)

val interface: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parsetree.signature)

val implementation: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parsetree.structure)

module MenhirInterpreter : sig
  
  (* The incremental API. *)
  
  include MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE
    with type token = token
  
  (* The indexed type of terminal symbols. *)
  
  type _ terminal = 
    | T_error : unit terminal
    | T_WITH : unit terminal
    | T_WHILE : unit terminal
    | T_WHEN : unit terminal
    | T_VIRTUAL : unit terminal
    | T_VAL : unit terminal
    | T_UNIQUE : unit terminal
    | T_UNDERSCORE : unit terminal
    | T_UIDENT : (string) terminal
    | T_TYPE : unit terminal
    | T_TRY : unit terminal
    | T_TRUE : unit terminal
    | T_TO : unit terminal
    | T_TILDE : unit terminal
    | T_THEN : unit terminal
    | T_STRUCT : unit terminal
    | T_STRING : (string * Location.t * string option) terminal
    | T_STAR : unit terminal
    | T_STACK : unit terminal
    | T_SIG : unit terminal
    | T_SEMISEMI : unit terminal
    | T_SEMI : unit terminal
    | T_RPAREN : unit terminal
    | T_REC : unit terminal
    | T_RBRACKET : unit terminal
    | T_RBRACE : unit terminal
    | T_QUOTED_STRING_ITEM : (string * Location.t * string * Location.t * string option) terminal
    | T_QUOTED_STRING_EXPR : (string * Location.t * string * Location.t * string option) terminal
    | T_QUOTE : unit terminal
    | T_QUESTION : unit terminal
    | T_PRIVATE : unit terminal
    | T_PREFIXOP : (string) terminal
    | T_PLUSEQ : unit terminal
    | T_PLUSDOT : unit terminal
    | T_PLUS : unit terminal
    | T_PERCENT : unit terminal
    | T_OVERWRITE : unit terminal
    | T_OR : unit terminal
    | T_OPTLABEL : (string) terminal
    | T_OPEN : unit terminal
    | T_ONCE : unit terminal
    | T_OF : unit terminal
    | T_OBJECT : unit terminal
    | T_NONREC : unit terminal
    | T_NEW : unit terminal
    | T_MUTABLE : unit terminal
    | T_MODULE : unit terminal
    | T_MOD : unit terminal
    | T_MINUSGREATER : unit terminal
    | T_MINUSDOT : unit terminal
    | T_MINUS : unit terminal
    | T_METHOD : unit terminal
    | T_MATCH : unit terminal
    | T_LPAREN : unit terminal
    | T_LOCAL : unit terminal
    | T_LIDENT : (string) terminal
    | T_LETOP : (string) terminal
    | T_LET : unit terminal
    | T_LESSMINUS : unit terminal
    | T_LESS : unit terminal
    | T_LBRACKETPERCENTPERCENT : unit terminal
    | T_LBRACKETPERCENT : unit terminal
    | T_LBRACKETLESS : unit terminal
    | T_LBRACKETGREATER : unit terminal
    | T_LBRACKETCOLON : unit terminal
    | T_LBRACKETBAR : unit terminal
    | T_LBRACKETATATAT : unit terminal
    | T_LBRACKETATAT : unit terminal
    | T_LBRACKETAT : unit terminal
    | T_LBRACKET : unit terminal
    | T_LBRACELESS : unit terminal
    | T_LBRACE : unit terminal
    | T_LAZY : unit terminal
    | T_LABEL : (string) terminal
    | T_KIND_OF : unit terminal
    | T_KIND_ABBREV : unit terminal
    | T_INT : (string * char option) terminal
    | T_INITIALIZER : unit terminal
    | T_INHERIT : unit terminal
    | T_INFIXOP4 : (string) terminal
    | T_INFIXOP3 : (string) terminal
    | T_INFIXOP2 : (string) terminal
    | T_INFIXOP1 : (string) terminal
    | T_INFIXOP0 : (string) terminal
    | T_INCLUDE : unit terminal
    | T_IN : unit terminal
    | T_IF : unit terminal
    | T_HASH_SUFFIX : unit terminal
    | T_HASH_INT : (string * char option) terminal
    | T_HASH_FLOAT : (string * char option) terminal
    | T_HASHOP : (string) terminal
    | T_HASHLPAREN : unit terminal
    | T_HASHLBRACE : unit terminal
    | T_HASH : unit terminal
    | T_GREATERRBRACKET : unit terminal
    | T_GREATERRBRACE : unit terminal
    | T_GREATERDOT : unit terminal
    | T_GREATER : unit terminal
    | T_GLOBAL : unit terminal
    | T_FUNCTOR : unit terminal
    | T_FUNCTION : unit terminal
    | T_FUN : unit terminal
    | T_FOR : unit terminal
    | T_FLOAT : (string * char option) terminal
    | T_FALSE : unit terminal
    | T_EXTERNAL : unit terminal
    | T_EXCLAVE : unit terminal
    | T_EXCEPTION : unit terminal
    | T_EQUAL : unit terminal
    | T_EOL : unit terminal
    | T_EOF : unit terminal
    | T_END : unit terminal
    | T_ELSE : unit terminal
    | T_DOWNTO : unit terminal
    | T_DOTTILDE : unit terminal
    | T_DOTOP : (string) terminal
    | T_DOTLESS : unit terminal
    | T_DOTHASH : unit terminal
    | T_DOTDOT : unit terminal
    | T_DOT : unit terminal
    | T_DONE : unit terminal
    | T_DOCSTRING : (Docstrings.docstring) terminal
    | T_DO : unit terminal
    | T_CONSTRAINT : unit terminal
    | T_COMMENT : (string * Location.t) terminal
    | T_COMMA : unit terminal
    | T_COLONRBRACKET : unit terminal
    | T_COLONGREATER : unit terminal
    | T_COLONEQUAL : unit terminal
    | T_COLONCOLON : unit terminal
    | T_COLON : unit terminal
    | T_CLASS : unit terminal
    | T_CHAR : (char) terminal
    | T_BEGIN : unit terminal
    | T_BARRBRACKET : unit terminal
    | T_BARBAR : unit terminal
    | T_BAR : unit terminal
    | T_BANG : unit terminal
    | T_BACKQUOTE : unit terminal
    | T_ATAT : unit terminal
    | T_AT : unit terminal
    | T_ASSERT : unit terminal
    | T_AS : unit terminal
    | T_ANDOP : (string) terminal
    | T_AND : unit terminal
    | T_AMPERSAND : unit terminal
    | T_AMPERAMPER : unit terminal
  
  (* The indexed type of nonterminal symbols. *)
  
  type _ nonterminal = 
    | N_with_type_binder : (Asttypes.private_flag) nonterminal
    | N_with_constraint : (Parsetree.with_constraint) nonterminal
    | N_virtual_with_private_flag : (Asttypes.private_flag) nonterminal
    | N_virtual_with_mutable_flag : (Asttypes.mutable_flag) nonterminal
    | N_virtual_flag : (Asttypes.virtual_flag) nonterminal
    | N_value_description : (Parsetree.value_description * string Location.loc option) nonterminal
    | N_value_constant : (Parsetree.constant) nonterminal
    | N_value : ((string Location.loc * Asttypes.mutable_flag * Parsetree.class_field_kind) *
  Parsetree.attributes) nonterminal
    | N_val_longident : (Longident.t) nonterminal
    | N_val_ident : (string) nonterminal
    | N_val_extra_ident : (string) nonterminal
    | N_use_file : (Parsetree.toplevel_phrase list) nonterminal
    | N_unboxed_constant : (Parsetree.constant) nonterminal
    | N_type_variance : (Asttypes.variance * Asttypes.injectivity) nonterminal
    | N_type_unboxed_longident : (Longident.t) nonterminal
    | N_type_trailing_no_hash : (string) nonterminal
    | N_type_trailing_hash : (string) nonterminal
    | N_type_parameters : ((Parsetree.core_type * (Asttypes.variance * Asttypes.injectivity)) list) nonterminal
    | N_type_parameter : (Parsetree.core_type * (Asttypes.variance * Asttypes.injectivity)) nonterminal
    | N_type_longident : (Longident.t) nonterminal
    | N_type_kind : (Parsetree.type_kind * Asttypes.private_flag * Parsetree.core_type option) nonterminal
    | N_type_constraint : (Parsetree.type_constraint) nonterminal
    | N_tuple_type : (Parsetree.core_type) nonterminal
    | N_toplevel_phrase : (Parsetree.toplevel_phrase) nonterminal
    | N_toplevel_directive : (Parsetree.toplevel_phrase) nonterminal
    | N_tag_field : (Parsetree.row_field) nonterminal
    | N_subtractive : (string) nonterminal
    | N_structure_item : (Parsetree.structure_item) nonterminal
    | N_structure : (Parsetree.structure) nonterminal
    | N_strict_function_or_labeled_tuple_type : (Parsetree.core_type) nonterminal
    | N_strict_binding_modes : (Parsetree.modes -> Parsetree.expression) nonterminal
    | N_str_exception_declaration : (Parsetree.type_exception * string Location.loc option) nonterminal
    | N_single_attr_id : (string) nonterminal
    | N_simple_pattern_not_ident : (Parsetree.pattern) nonterminal
    | N_simple_pattern_extend_modes_or_poly : (Parsetree.pattern) nonterminal
    | N_simple_pattern : (Parsetree.pattern) nonterminal
    | N_simple_expr : (Parsetree.expression) nonterminal
    | N_simple_delimited_pattern : (Parsetree.pattern) nonterminal
    | N_signed_value_constant : (Parsetree.constant) nonterminal
    | N_signed_constant : (Parsetree.constant) nonterminal
    | N_signature_item : (Parsetree.signature_item) nonterminal
    | N_signature : (Parsetree.signature) nonterminal
    | N_sig_exception_declaration : (Parsetree.type_exception * string Location.loc option) nonterminal
    | N_seq_expr : (Parsetree.expression) nonterminal
    | N_separated_or_terminated_nonempty_list_SEMI_record_expr_field_ : ((Longident.t Location.loc * Parsetree.expression) list) nonterminal
    | N_separated_or_terminated_nonempty_list_SEMI_pattern_ : (Parsetree.pattern list) nonterminal
    | N_separated_or_terminated_nonempty_list_SEMI_object_expr_field_ : ((string Location.loc * Parsetree.expression) list) nonterminal
    | N_separated_or_terminated_nonempty_list_SEMI_expr_ : (Parsetree.expression list) nonterminal
    | N_row_field : (Parsetree.row_field) nonterminal
    | N_reversed_separated_nontrivial_llist_COMMA_one_type_parameter_of_several_ : (Parsetree.core_type list) nonterminal
    | N_reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_ : ((string option * Parsetree.core_type) list) nonterminal
    | N_reversed_separated_nonempty_llist_STAR_constructor_argument_ : (Parsetree.constructor_argument list) nonterminal
    | N_reversed_separated_nonempty_llist_COMMA_type_parameter_ : ((Parsetree.core_type * (Asttypes.variance * Asttypes.injectivity)) list) nonterminal
    | N_reversed_separated_nonempty_llist_COMMA_parenthesized_type_parameter_ : ((Parsetree.core_type * (Asttypes.variance * Asttypes.injectivity)) list) nonterminal
    | N_reversed_separated_nonempty_llist_COMMA_core_type_ : (Parsetree.core_type list) nonterminal
    | N_reversed_separated_nonempty_llist_BAR_row_field_ : (Parsetree.row_field list) nonterminal
    | N_reversed_separated_nonempty_llist_AND_with_constraint_ : (Parsetree.with_constraint list) nonterminal
    | N_reversed_separated_nonempty_llist_AND_comprehension_clause_binding_ : (Parsetree.comprehension_clause_binding list) nonterminal
    | N_reversed_separated_nonempty_llist_AMPERSAND_core_type_no_attr_ : (Parsetree.core_type list) nonterminal
    | N_reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ : (Parsetree.case list) nonterminal
    | N_reversed_nonempty_llist_typevar_ : ((string Location.loc * Parsetree.jkind_annotation option) list) nonterminal
    | N_reversed_nonempty_llist_name_tag_ : (string list) nonterminal
    | N_reversed_nonempty_llist_labeled_simple_expr_ : ((Parsetree.arg_label * Parsetree.expression) list) nonterminal
    | N_reversed_nonempty_llist_functor_arg_ : ((Lexing.position * Parsetree.functor_parameter) list) nonterminal
    | N_reversed_nonempty_llist_comprehension_clause_ : (Parsetree.comprehension_clause list) nonterminal
    | N_reversed_nonempty_concat_fun_param_as_list_ : (Parsetree.function_param list) nonterminal
    | N_reversed_llist_preceded_CONSTRAINT_constrain__ : ((Parsetree.core_type * Parsetree.core_type * Location.t) list) nonterminal
    | N_reversed_labeled_tuple_pattern_pattern_no_exn_ : (Asttypes.closed_flag * (string option * Parsetree.pattern) list) nonterminal
    | N_reversed_labeled_tuple_pattern_pattern_ : (Asttypes.closed_flag * (string option * Parsetree.pattern) list) nonterminal
    | N_reversed_labeled_tuple_body : ((string option * Parsetree.expression) list) nonterminal
    | N_reversed_bar_llist_extension_constructor_declaration_ : (Parsetree.extension_constructor list) nonterminal
    | N_reversed_bar_llist_extension_constructor_ : (Parsetree.extension_constructor list) nonterminal
    | N_reversed_bar_llist_constructor_declaration_ : (Parsetree.constructor_declaration list) nonterminal
    | N_reverse_product_jkind : (Parsetree.jkind_annotation list) nonterminal
    | N_record_expr_content : (Parsetree.expression option *
  (Longident.t Location.loc * Parsetree.expression) list) nonterminal
    | N_rec_flag : (Asttypes.rec_flag) nonterminal
    | N_private_virtual_flags : (Asttypes.private_flag * Asttypes.virtual_flag) nonterminal
    | N_private_flag : (Asttypes.private_flag) nonterminal
    | N_primitive_declaration : (Parsetree.value_description * string Location.loc option) nonterminal
    | N_post_item_attribute : (Parsetree.attribute) nonterminal
    | N_possibly_poly_core_type_no_attr_ : (Parsetree.core_type) nonterminal
    | N_possibly_poly_core_type_ : (Parsetree.core_type) nonterminal
    | N_payload : (Parsetree.payload) nonterminal
    | N_pattern_with_modes_or_poly : (Parsetree.pattern) nonterminal
    | N_pattern_var : (Parsetree.pattern) nonterminal
    | N_pattern_no_exn : (Parsetree.pattern) nonterminal
    | N_pattern_gen : (Parsetree.pattern) nonterminal
    | N_pattern : (Parsetree.pattern) nonterminal
    | N_parse_val_longident : (Longident.t) nonterminal
    | N_parse_pattern : (Parsetree.pattern) nonterminal
    | N_parse_mty_longident : (Longident.t) nonterminal
    | N_parse_module_type : (Parsetree.module_type) nonterminal
    | N_parse_module_expr : (Parsetree.module_expr) nonterminal
    | N_parse_mod_longident : (Longident.t) nonterminal
    | N_parse_mod_ext_longident : (Longident.t) nonterminal
    | N_parse_expression : (Parsetree.expression) nonterminal
    | N_parse_core_type : (Parsetree.core_type) nonterminal
    | N_parse_constr_longident : (Longident.t) nonterminal
    | N_parse_any_longident : (Longident.t) nonterminal
    | N_parenthesized_type_parameter : (Parsetree.core_type * (Asttypes.variance * Asttypes.injectivity)) nonterminal
    | N_paren_module_expr : (Parsetree.module_expr) nonterminal
    | N_optlabel : (string) nonterminal
    | N_optional_poly_type_and_modes : (Parsetree.core_type option * Parsetree.modes) nonterminal
    | N_optional_atomic_constraint_ : (Parsetree.function_constraint) nonterminal
    | N_optional_atat_modalities_expr : (Parsetree.modalities) nonterminal
    | N_option_type_constraint_ : (Parsetree.type_constraint option) nonterminal
    | N_option_preceded_EQUAL_seq_expr__ : (Parsetree.expression option) nonterminal
    | N_option_preceded_EQUAL_pattern__ : (Parsetree.pattern option) nonterminal
    | N_option_preceded_EQUAL_module_type__ : (Parsetree.module_type option) nonterminal
    | N_option_preceded_EQUAL_expr__ : (Parsetree.expression option) nonterminal
    | N_option_preceded_COLON_core_type__ : (Parsetree.core_type option) nonterminal
    | N_option_preceded_AS_mkrhs_LIDENT___ : (string Location.loc option) nonterminal
    | N_option_jkind_constraint_ : (Parsetree.jkind_annotation option) nonterminal
    | N_option_constraint__ : ((Parsetree.type_constraint option * Parsetree.modes) option) nonterminal
    | N_option_SEMI_ : (unit option) nonterminal
    | N_option_BAR_ : (unit option) nonterminal
    | N_opt_ampersand : (bool) nonterminal
    | N_operator : (string) nonterminal
    | N_open_description : (Longident.t Location.loc Parsetree.open_infos * string Location.loc option) nonterminal
    | N_open_declaration : (Parsetree.module_expr Parsetree.open_infos * string Location.loc option) nonterminal
    | N_object_type : (Parsetree.core_type) nonterminal
    | N_nonempty_type_kind : (Parsetree.type_kind * Asttypes.private_flag * Parsetree.core_type option) nonterminal
    | N_nonempty_list_raw_string_ : (string list) nonterminal
    | N_nonempty_list_newtype_ : ((string Location.loc * Parsetree.jkind_annotation option) list) nonterminal
    | N_nonempty_list_mode_legacy_ : (Parsetree.modes) nonterminal
    | N_nonempty_list_mode_ : (Parsetree.modes) nonterminal
    | N_nonempty_list_modality_ : (Parsetree.modalities) nonterminal
    | N_nonempty_list_mkrhs_LIDENT__ : (string Location.loc list) nonterminal
    | N_newtypes : ((string Location.loc * Parsetree.jkind_annotation option) list) nonterminal
    | N_newtype : (string Location.loc * Parsetree.jkind_annotation option) nonterminal
    | N_name_tag : (string) nonterminal
    | N_mutable_virtual_flags : (Asttypes.mutable_flag * Asttypes.virtual_flag) nonterminal
    | N_mutable_or_global_flag : (Asttypes.mutable_flag * Parsetree.modality Location.loc list) nonterminal
    | N_mutable_flag : (Asttypes.mutable_flag) nonterminal
    | N_mty_longident : (Longident.t) nonterminal
    | N_module_type_subst : (Parsetree.module_type_declaration * string Location.loc option) nonterminal
    | N_module_type_declaration : (Parsetree.module_type_declaration * string Location.loc option) nonterminal
    | N_module_type_atomic : (Parsetree.module_type) nonterminal
    | N_module_type : (Parsetree.module_type) nonterminal
    | N_module_subst : (Parsetree.module_substitution * string Location.loc option) nonterminal
    | N_module_name_modal_atat_modalities_expr_ : (string option Location.loc * Parsetree.modalities) nonterminal
    | N_module_name_modal_at_mode_expr_ : (Ocaml_parsing.Ast_helper.str_opt * Parsetree.modes) nonterminal
    | N_module_name : (string option) nonterminal
    | N_module_expr : (Parsetree.module_expr) nonterminal
    | N_module_declaration_body_module_type_with_optional_modes_ : (Parsetree.module_type * Parsetree.modes) nonterminal
    | N_module_declaration_body___anonymous_8_ : (Parsetree.module_type * Parsetree.modalities) nonterminal
    | N_module_binding_body : (Parsetree.module_expr) nonterminal
    | N_mod_longident : (Longident.t) nonterminal
    | N_mod_ext_longident : (Longident.t) nonterminal
    | N_mk_longident_mod_longident_val_ident_ : (Longident.t) nonterminal
    | N_mk_longident_mod_longident_UIDENT_ : (Longident.t) nonterminal
    | N_mk_longident_mod_longident_LIDENT_ : (Longident.t) nonterminal
    | N_mk_longident_mod_ext_longident_type_trailing_no_hash_ : (Longident.t) nonterminal
    | N_mk_longident_mod_ext_longident_type_trailing_hash_ : (Longident.t) nonterminal
    | N_mk_longident_mod_ext_longident_ident_ : (Longident.t) nonterminal
    | N_mk_longident_mod_ext_longident___anonymous_50_ : (Longident.t) nonterminal
    | N_mk_longident_mod_ext_longident_UIDENT_ : (Longident.t) nonterminal
    | N_mk_longident_mod_ext_longident_LIDENT_ : (Longident.t) nonterminal
    | N_method_ : ((string Location.loc * Asttypes.private_flag * Parsetree.class_field_kind) *
  Parsetree.attributes) nonterminal
    | N_meth_list : (Parsetree.object_field list * Asttypes.closed_flag) nonterminal
    | N_match_case : (Parsetree.case) nonterminal
    | N_listx_SEMI_record_pat_field_UNDERSCORE_ : ((Longident.t Location.loc * Parsetree.pattern) list * unit option) nonterminal
    | N_list_use_file_element_ : (Parsetree.toplevel_phrase list list) nonterminal
    | N_list_text_str_structure_item__ : (Parsetree.structure_item list list) nonterminal
    | N_list_text_cstr_class_field__ : (Parsetree.class_field list list) nonterminal
    | N_list_text_csig_class_sig_field__ : (Parsetree.class_type_field list list) nonterminal
    | N_list_structure_element_ : (Parsetree.structure_item list list) nonterminal
    | N_list_signature_element_ : (Parsetree.signature_item list list) nonterminal
    | N_list_post_item_attribute_ : (Parsetree.attributes) nonterminal
    | N_list_generic_and_type_declaration_type_subst_kind__ : (Parsetree.type_declaration list) nonterminal
    | N_list_generic_and_type_declaration_type_kind__ : (Parsetree.type_declaration list) nonterminal
    | N_list_attribute_ : (Parsetree.attributes) nonterminal
    | N_list_and_module_declaration_ : (Parsetree.module_declaration list) nonterminal
    | N_list_and_module_binding_ : (Parsetree.module_binding list) nonterminal
    | N_list_and_class_type_declaration_ : (Parsetree.class_type Parsetree.class_infos list) nonterminal
    | N_list_and_class_description_ : (Parsetree.class_type Parsetree.class_infos list) nonterminal
    | N_list_and_class_declaration_ : (Parsetree.class_expr Parsetree.class_infos list) nonterminal
    | N_letop_bindings : (Parsetree.pattern * Parsetree.expression * Parsetree.binding_op list) nonterminal
    | N_letop_binding_body : (Parsetree.pattern * Parsetree.expression) nonterminal
    | N_let_pattern : (Parsetree.pattern) nonterminal
    | N_let_bindings_no_ext_ : (Parser_types.let_bindings) nonterminal
    | N_let_bindings_ext_ : (Parser_types.let_bindings) nonterminal
    | N_let_binding_body_no_punning : (Parsetree.pattern * Parsetree.expression *
  Parsetree.value_constraint option * Parsetree.modes) nonterminal
    | N_let_binding_body : (Parsetree.pattern * Parsetree.expression *
  Parsetree.value_constraint option * Parsetree.modes * bool) nonterminal
    | N_labeled_tuple_pat_element_list_pattern_no_exn_ : ((string option * Parsetree.pattern) list) nonterminal
    | N_labeled_tuple_pat_element_list_pattern_ : ((string option * Parsetree.pattern) list) nonterminal
    | N_labeled_simple_pattern : (Parsetree.arg_label * Parsetree.expression option * Parsetree.pattern) nonterminal
    | N_labeled_simple_expr : (Parsetree.arg_label * Parsetree.expression) nonterminal
    | N_label_longident : (Longident.t) nonterminal
    | N_label_let_pattern : (string * Parsetree.pattern) nonterminal
    | N_label_declarations : (Parsetree.label_declaration list) nonterminal
    | N_label_declaration_semi : (Parsetree.label_declaration) nonterminal
    | N_label_declaration : (Parsetree.label_declaration) nonterminal
    | N_kind_abbreviation_decl : (string Location.loc * Parsetree.jkind_annotation) nonterminal
    | N_jkind_desc : (Parsetree.jkind_annotation_desc) nonterminal
    | N_jkind_constraint : (Parsetree.jkind_annotation) nonterminal
    | N_jkind_annotation : (Parsetree.jkind_annotation) nonterminal
    | N_item_extension : (Parsetree.extension) nonterminal
    | N_interface : (Parsetree.signature) nonterminal
    | N_index_mod : (string) nonterminal
    | N_include_kind : (Parsetree.include_kind) nonterminal
    | N_implementation : (Parsetree.structure) nonterminal
    | N_ident : (string) nonterminal
    | N_generic_type_declaration_nonrec_flag_type_kind_ : ((Asttypes.rec_flag * string Location.loc option) *
  Parsetree.type_declaration) nonterminal
    | N_generic_type_declaration_no_nonrec_flag_type_subst_kind_ : ((Asttypes.rec_flag * string Location.loc option) *
  Parsetree.type_declaration) nonterminal
    | N_generic_constructor_declaration_epsilon_ : (Ocaml_parsing.Ast_helper.str *
  (string Location.loc * Parsetree.jkind_annotation option) list *
  Parsetree.constructor_arguments * Parsetree.core_type option *
  Parsetree.attributes * Location.t * Ocaml_parsing.Docstrings.info) nonterminal
    | N_generic_constructor_declaration_BAR_ : (Ocaml_parsing.Ast_helper.str *
  (string Location.loc * Parsetree.jkind_annotation option) list *
  Parsetree.constructor_arguments * Parsetree.core_type option *
  Parsetree.attributes * Location.t * Ocaml_parsing.Docstrings.info) nonterminal
    | N_generalized_constructor_arguments : ((string Location.loc * Parsetree.jkind_annotation option) list *
  Parsetree.constructor_arguments * Parsetree.core_type option) nonterminal
    | N_functor_args : ((Lexing.position * Parsetree.functor_parameter) list) nonterminal
    | N_functor_arg : (Lexing.position * Parsetree.functor_parameter) nonterminal
    | N_function_type : (Parsetree.core_type) nonterminal
    | N_fun_seq_expr : (Parsetree.expression) nonterminal
    | N_fun_params : (Parsetree.function_param list) nonterminal
    | N_fun_param_as_list : (Parsetree.function_param list) nonterminal
    | N_fun_expr : (Parsetree.expression) nonterminal
    | N_fun_body : (Parsetree.function_body) nonterminal
    | N_fun_ : (Parsetree.expression) nonterminal
    | N_formal_class_parameters : ((Parsetree.core_type * (Asttypes.variance * Asttypes.injectivity)) list) nonterminal
    | N_floating_attribute : (Parsetree.attribute) nonterminal
    | N_extension_type : (Parsetree.core_type) nonterminal
    | N_extension_constructor_rebind_epsilon_ : (Parsetree.extension_constructor) nonterminal
    | N_extension_constructor_rebind_BAR_ : (Parsetree.extension_constructor) nonterminal
    | N_extension : (Parsetree.extension) nonterminal
    | N_ext : (string Location.loc option) nonterminal
    | N_direction_flag : (Asttypes.direction_flag) nonterminal
    | N_delimited_type_supporting_local_open : (Parsetree.core_type) nonterminal
    | N_delimited_type : (Parsetree.core_type) nonterminal
    | N_core_type : (Parsetree.core_type) nonterminal
    | N_constructor_declarations : (Parsetree.constructor_declaration list) nonterminal
    | N_constructor_arguments : (Parsetree.constructor_arguments) nonterminal
    | N_constrain_field : (Parsetree.core_type * Parsetree.core_type) nonterminal
    | N_constr_longident : (Longident.t) nonterminal
    | N_constr_ident : (string) nonterminal
    | N_constr_extra_nonprefix_ident : (string) nonterminal
    | N_constant : (Parsetree.constant) nonterminal
    | N_comprehension_iterator : (Parsetree.comprehension_iterator) nonterminal
    | N_comprehension_clause_binding : (Parsetree.comprehension_clause_binding) nonterminal
    | N_comprehension_clause : (Parsetree.comprehension_clause) nonterminal
    | N_clty_longident : (Longident.t) nonterminal
    | N_class_type_declarations : (string Location.loc option * Parsetree.class_type_declaration list) nonterminal
    | N_class_type : (Parsetree.class_type) nonterminal
    | N_class_simple_expr : (Parsetree.class_expr) nonterminal
    | N_class_signature : (Parsetree.class_type) nonterminal
    | N_class_sig_field : (Parsetree.class_type_field) nonterminal
    | N_class_self_type : (Parsetree.core_type) nonterminal
    | N_class_self_pattern : (Parsetree.pattern) nonterminal
    | N_class_longident : (Longident.t) nonterminal
    | N_class_fun_def : (Parsetree.class_expr) nonterminal
    | N_class_fun_binding : (Parsetree.class_expr) nonterminal
    | N_class_field : (Parsetree.class_field) nonterminal
    | N_class_expr : (Parsetree.class_expr) nonterminal
    | N_attribute : (Parsetree.attribute) nonterminal
    | N_attr_payload : (Parsetree.payload) nonterminal
    | N_attr_id : (string Location.loc) nonterminal
    | N_atomic_type : (Parsetree.core_type) nonterminal
    | N_atat_modalities_expr : (Parsetree.modalities) nonterminal
    | N_at_mode_expr : (Parsetree.modes) nonterminal
    | N_any_longident : (Longident.t) nonterminal
    | N_and_let_binding : (Parser_types.let_binding) nonterminal
    | N_alias_type : (Parsetree.core_type) nonterminal
    | N_additive : (string) nonterminal
  
  (* The inspection API. *)
  
  include MenhirLib.IncrementalEngine.INSPECTION
    with type 'a lr1state := 'a lr1state
    with type production := production
    with type 'a terminal := 'a terminal
    with type 'a nonterminal := 'a nonterminal
    with type 'a env := 'a env
  
end

(* The entry point(s) to the incremental API. *)

module Incremental : sig
  
  val use_file: Lexing.position -> (Parsetree.toplevel_phrase list) MenhirInterpreter.checkpoint
  
  val toplevel_phrase: Lexing.position -> (Parsetree.toplevel_phrase) MenhirInterpreter.checkpoint
  
  val parse_val_longident: Lexing.position -> (Longident.t) MenhirInterpreter.checkpoint
  
  val parse_pattern: Lexing.position -> (Parsetree.pattern) MenhirInterpreter.checkpoint
  
  val parse_mty_longident: Lexing.position -> (Longident.t) MenhirInterpreter.checkpoint
  
  val parse_module_type: Lexing.position -> (Parsetree.module_type) MenhirInterpreter.checkpoint
  
  val parse_module_expr: Lexing.position -> (Parsetree.module_expr) MenhirInterpreter.checkpoint
  
  val parse_mod_longident: Lexing.position -> (Longident.t) MenhirInterpreter.checkpoint
  
  val parse_mod_ext_longident: Lexing.position -> (Longident.t) MenhirInterpreter.checkpoint
  
  val parse_expression: Lexing.position -> (Parsetree.expression) MenhirInterpreter.checkpoint
  
  val parse_core_type: Lexing.position -> (Parsetree.core_type) MenhirInterpreter.checkpoint
  
  val parse_constr_longident: Lexing.position -> (Longident.t) MenhirInterpreter.checkpoint
  
  val parse_any_longident: Lexing.position -> (Longident.t) MenhirInterpreter.checkpoint
  
  val interface: Lexing.position -> (Parsetree.signature) MenhirInterpreter.checkpoint
  
  val implementation: Lexing.position -> (Parsetree.structure) MenhirInterpreter.checkpoint
  
end
