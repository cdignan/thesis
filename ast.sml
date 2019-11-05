structure AST = struct

  datatype term
    (* list of cid * attribute * type * notnull * dflt_val * pk *)
    (* all other terms evaluate to a relation *)
    = Relation of ((string * int) * (string * string) * (string * string) *
                  (string * int) * (string * string) * (string * int) * (string * string list)) list
    | CartProd of (term * term)
    | NatJoin of (term * term)
    | Proj of (string list * term)

end
