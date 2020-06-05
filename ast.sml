structure AST = struct

  datatype types
    = Int
    | Text
    | Date
    | Time

  type uniq = string list list

  type pk = string list

  type fk = {table: string, from: string, to: string} list list

  type attr = {attribute: string, ty: types, notnull: bool, dflt_val: string, tables: string list}

  datatype term
    (* list of cid * attribute * type * notnull * dflt_val * pk *)
    (* all other terms evaluate to a relation *)
    = Relation of attr list * uniq * pk option * fk option
    | CartProd of (term * term * ((string * string) list))
    (* first string is original attribute, second string is renamed attribute,
       if bool is true then select distinct *)
    | Proj of ((string * string * bool) list * term)
    | Union of (term * term)

end
