structure Eval : sig

  val eval : Ty.ty * string list list -> AST.term list

end =  struct

  fun concat (Ty.Record [], []) = []
    | concat (Ty.Record ((x,y)::list1), z::list2) = (x, y, z)::(concat (Ty.Record list1, list2))
    | concat (_, _) = raise Fail "Eval.concat improper input"

  fun fullConcat (record, []) = []
    | fullConcat (record, row::rows) = (concat (record, row))::(fullConcat (record, rows))

  fun helper (x, Ty.Int, z) = (x, AST.Int z)
    | helper (x, Ty.Text, z) = (x, AST.Text z)
    | helper (x, Ty.Date, z) = (x, AST.Date z)
    | helper (x, Ty.Time, z) = (x, AST.Time z)
    | helper (_, _, _) = raise Fail "no support for this type.... yet"

  (* take in record type and string list list, return list of record terms *)
  fun eval (record, rows) =
    let
      val i = fullConcat (record, rows)
      fun res [] = []
        | res (row::rows) = (AST.Record (List.map helper row))::(res rows)
    in
      res i
    end

end
