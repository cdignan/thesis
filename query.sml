structure Query = struct

  (* type Query.run (db, cmd) in the SML repl *)

  (* takes in a string list list, returns a Ty.ty list list
     everything will be Ty.Text but that will be corrected in convert *)
  fun toText [] = []
    | toText (l :: ls) = (List.map (fn x => Ty.Type (Ty.Text x)) l) :: toText ls

  (* take in one attribute with it's info, and one list of Ty.Text terms
     return corrected list of Ty.ty terms *)
  fun conv (_, _, Ty.Type (Ty.Text "")) = Ty.Option NONE
    | conv (e : AST.attr, p : AST.pk option, Ty.Type (Ty.Text x)) =
        (case (#ty e, #notnull e, #attribute e, p)
          of (AST.Int, true, _, _) =>
               (case Int.fromString x
                 of SOME x' => Ty.Type (Ty.Int x')
                  | NONE => raise Fail "expected int")
           | (AST.Int, _, a, SOME strs) =>
               (case Int.fromString x
                 of SOME x' => if (List.exists (fn z => z = a) strs)
                               then Ty.Type (Ty.Int x')
                               else Ty.Option (SOME (Ty.Int x'))
                  | NONE => raise Fail "expected int")
           | (AST.Int, _, _, _) =>
               (case Int.fromString x
                 of SOME x' => Ty.Option (SOME (Ty.Int x'))
                  | NONE => Ty.Option NONE)
           | (AST.Text, true, _, _) => Ty.Type (Ty.Text x)
           | (AST.Text, _, a, SOME strs) => if (List.exists (fn z => z = a) strs)
                                            then Ty.Type (Ty.Text x)
                                            else Ty.Option (SOME (Ty.Text x))
           | (AST.Text, _, _, _) => Ty.Option (SOME (Ty.Text x))
           | (AST.Date, true, _, _) => Ty.Type (Ty.Date x)
           | (AST.Date, _, a, SOME strs) => if (List.exists (fn z => z = a) strs)
                                            then Ty.Type (Ty.Date x)
                                            else Ty.Option (SOME (Ty.Date x))
           | (AST.Date, _, _, _) => Ty.Option (SOME (Ty.Date x))
           | (AST.Time, true, _, _) => Ty.Type (Ty.Time x)
           | (AST.Time, _, a, SOME strs) => if (List.exists (fn z => z = a) strs)
                                            then Ty.Type (Ty.Time x)
                                            else Ty.Option (SOME (Ty.Time x))
           | (AST.Time, _, _, _) => Ty.Option (SOME (Ty.Time x)))
    | conv (_, _, _) = raise Fail "invalid input"

  fun convert ([], _, []) = []
    | convert (e :: es : AST.attr list, p : AST.pk option, l :: ls) = (conv (e, p, l)) :: convert (es, p, ls)
    | convert (_, _, _) = raise Fail "invalid convert"

  (* convert a list of lists *)
  fun correctTypes (_, _, []) = []
    | correctTypes (t, p, l :: ls) =
        (convert (t, p, l)) :: correctTypes (t, p, ls)

  fun run (db, cmd) =
    let
      val _ = OS.Process.system ("sqlite3 -csv -noheader " ^ db ^ " " ^ "\"" ^ cmd ^ "\"" ^ " > result.csv")
      val rows = toText (Scan.readlist "result.csv")
      val scan = Scan.scan cmd
      val parse = Parse.parse (db, scan)
      val eval = Eval.eval parse
      val convertedRows =
            (case eval
              of AST.Relation (l, _, p, _) => correctTypes (l, p, rows)
               | _ => raise Fail "should evaluate to relation")
    in
      (eval, convertedRows)
    end

end
