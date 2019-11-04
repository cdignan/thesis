structure AST = struct

  datatype term
    = Relation of (int * string * string * int * string * int) list
    | CartProd of (term * term)
    | NatJoin of (term * term)
    | Proj of (string list * term)

end
