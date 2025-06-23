type t =
  | Expr
  | Module_path
  | Module_type
  | Patt
  | Type
  | Constant
  | Constructor
  | Label
  | Unknown
[@@deriving string, enumerate]
