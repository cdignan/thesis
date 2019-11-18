structure Test = struct

  (* type Test.run (db, cmd) in the SML repl *)

  (* takes in a string list list, returns a Ty.ty list list
     everything will be Ty.Text but that will be corrected in covert *)
  fun toText [] = []
    | toText (l :: ls) = (List.map (fn x => Ty.Type (Ty.Text x)) l) :: toText ls

  (* take in one attribute with it's info, and one list of Ty.Text terms
     return corrected list of Ty.ty terms *)
  fun convert (e, l) =
    let
      fun conv (_, (Ty.Type (Ty.Text "")) :: xs, 0) = (Ty.Option NONE) :: xs
        | conv (e : {cid: int, attribute: string, ty: string, notnull: bool, dflt_val: string,
                    primary_key: AST.pk option, tables: string list}, (Ty.Type (Ty.Text x)) :: xs, 0) =
            (case ((#ty e), (#notnull e), (#primary_key e))
              of ("INTEGER", true, _) =>
                   (case Int.fromString x
                     of SOME x' => (Ty.Type (Ty.Int x')) :: xs
                      | NONE => raise Fail "expected int")
               | ("INTEGER", _, SOME AST.PK) =>
                   (case Int.fromString x
                     of SOME x' => (Ty.Type (Ty.Int x')) :: xs
                      | NONE => raise Fail "expected int")
               | ("INTEGER", _, SOME (AST.FK _)) =>
                   (case Int.fromString x
                     of SOME x' => (Ty.Type (Ty.Int x')) :: xs
                      | NONE => raise Fail "expected int")
               | ("INTEGER", _, _) =>
                   (case Int.fromString x
                     of SOME x' => (Ty.Option (SOME (Ty.Int x'))) :: xs
                      | NONE => (Ty.Option NONE) :: xs)
               | ("TEXT", true, _) => (Ty.Type (Ty.Text x)) :: xs
               | ("TEXT", _, SOME AST.PK) => (Ty.Type (Ty.Text x)) :: xs
               | ("TEXT", _, SOME (AST.FK _)) => (Ty.Type (Ty.Text x)) :: xs
               | ("TEXT", _, _) => (Ty.Option (SOME (Ty.Text x))) :: xs
               | ("DATE", true, _) => (Ty.Type (Ty.Date x)) :: xs
               | ("DATE", _, SOME AST.PK) => (Ty.Type (Ty.Date x)) :: xs
               | ("DATE", _, SOME (AST.FK _)) => (Ty.Type (Ty.Date x)) :: xs
               | ("DATE", _, _) => (Ty.Option (SOME (Ty.Date x))) :: xs
               | ("TIME", true, _) => (Ty.Type (Ty.Time x)) :: xs
               | ("TIME", _, SOME AST.PK) => (Ty.Type (Ty.Time x)) :: xs
               | ("TIME", _, SOME (AST.FK _)) => (Ty.Type (Ty.Time x)) :: xs
               | ("TIME", _, _) => (Ty.Option (SOME (Ty.Time x))) :: xs
               | (_, _, _) => raise Fail "convert: type not supported")
        | conv (e, x :: xs, k) = x :: conv (e, xs, k - 1)
        | conv (_, _, _) = raise Fail "invalid input"
    in
      conv (e, l, #cid e)
    end

  (* convert a list of lists *)
  fun correctTypes (_, []) = []
    | correctTypes (t, l :: ls) =
        (convert (t, l)) :: correctTypes (t, ls)

  (* convert a list of lists for multiple attributes *)
  fun totalCorrectTypes ([], l) = l
    | totalCorrectTypes (t :: ts, l) =
        totalCorrectTypes (ts, correctTypes (t, l))

  fun run (db, cmd) =
    let
      val _ = OS.Process.system ("sqlite3 -csv -noheader " ^ db ^ " " ^ "\"" ^ cmd ^ "\"" ^ " > result.csv")
      val rows = toText (Scan.readlist "result.csv")
      val scan = Scan.scan cmd
      val parse = Parse.parse (db, scan)
      val eval = Eval.eval parse
      val convertedRows =
            (case eval
              of AST.Relation l => totalCorrectTypes (l, rows)
               | _ => raise Fail "should evaluate to relation")
    in
      (eval, convertedRows)
    end

end
