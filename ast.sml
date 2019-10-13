structure AST = struct

  datatype term
    = Int of string
    | Unit
    | Text of string
    | Date of string
    | Time of string
    | Record of (string * term) list

  datatype queryTerm
    = Select of string list * string list

end
