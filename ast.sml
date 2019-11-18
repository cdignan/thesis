structure AST = struct

  datatype pk
    = NotPK
    | PK of int

  datatype fk
    (* (seq * table * from * to * on_update * on_delete * match) *)
    = NotFK
    | FK of {seq: int, table: string, from: string, to: string,
            on_update: string, on_delete: string, matc: string}

  datatype term
    (* list of cid * attribute * type * notnull * dflt_val * pk *)
    (* all other terms evaluate to a relation *)
    = Relation of {cid: int, attribute: string, ty: string, notnull: bool, dflt_val: string,
                  primary_key: pk option, foreign_key: fk option, unique: bool, tables: string list} list
    | CartProd of (term * term)
    | NatJoin of (term * term)
    (* first string is original attribute, second string is renamed attribute *)
    | Proj of ((string * string) list * term)
    | Union of (term * term)

end
