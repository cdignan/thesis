structure AST = struct

  datatype term
    = Relation of (string * string) list
    | CartProd of (term * term)
    | NatJoin of (term * term)
    | Proj of (string list * term)

end
