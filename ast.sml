structure AST = struct

  datatype pk
    = NotPK
    | PK
    (* (seq * table * from * to * on_update * on_delete * match) *)
    | FK of (string * int) * (string * string) * (string * string) * (string * string) *
            (string * string) * (string * string) * (string * string)

  datatype term
    (* list of cid * attribute * type * notnull * dflt_val * pk *)
    (* all other terms evaluate to a relation *)
    = Relation of ((string * int) * (string * string) * (string * string) *
                  (string * bool) * (string * string) * (string * pk option) * (string * string list)) list
    | CartProd of (term * term)
    | NatJoin of (term * term)
    | Proj of (string list * term)

end
