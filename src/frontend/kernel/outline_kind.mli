type t =
  [ `Value
  | `Constructor
  | `Label
  | `Module
  | `Modtype
  | `Type
  | `Exn
  | `Class
  | `Method ]
[@@deriving string, equal, enumerate]
