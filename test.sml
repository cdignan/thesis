structure Test = struct

  (* type Test.run (db, cmd) in the SML repl *)

  (* takes in a string list list, returns a Ty.ty list list
     everything will be Ty.Text but that will be corrected in covert *)
  fun toText [] = []
    | toText (l :: ls) = (List.map (fn x => Ty.Text x) l) :: toText ls

  (* take in one attribute with it's info, and one list of Ty.Text terms
     return corrected list of Ty.ty terms *)
  fun convert ((n, b, c, d, e, f), l) =
    let
      fun conv ((n, b, "INTEGER", 1, e, f), (Ty.Text x) :: xs, 0) =
            (case Int.fromString x
              of SOME x' => (Ty.Int x') :: xs
               | NONE => raise Fail "expected int")
        | conv ((n, b, "INTEGER", d, e, 1), (Ty.Text x) :: xs, 0) =
            (case Int.fromString x
              of SOME x' => (Ty.Int x') :: xs
               | NONE => raise Fail "expected int")
        | conv ((n, b, "INTEGER", d, e, 2), (Ty.Text x) :: xs, 0) =
            (case Int.fromString x
              of SOME x' => (Ty.Int x') :: xs
               | NONE => raise Fail "expected int")
        | conv ((n, b, "INTEGER", d, e, f), (Ty.Text x) :: xs, 0) =
            (case Int.fromString x
              of SOME x' => (Ty.Option (Ty.Int x')) :: xs
               | NONE => raise Fail "expected int")
        | conv ((n, b, "TEXT", 1, e, f), x :: xs, 0) = x :: xs
        | conv ((n, b, "TEXT", d, e, 1), x :: xs, 0) = x :: xs
        | conv ((n, b, "TEXT", d, e, 2), x :: xs, 0) = x :: xs
        | conv ((n, b, "TEXT", d, e, f), x :: xs, 0) = (Ty.Option x) :: xs
        | conv ((n, b, "DATE", 1, e, f), (Ty.Text x) :: xs, 0) = (Ty.Date x) :: xs
        | conv ((n, b, "DATE", d, e, 1), (Ty.Text x) :: xs, 0) = (Ty.Date x) :: xs
        | conv ((n, b, "DATE", d, e, 2), (Ty.Text x) :: xs, 0) = (Ty.Date x) :: xs
        | conv ((n, b, "DATE", d, e, f), (Ty.Text x) :: xs, 0) = (Ty.Option (Ty.Date x)) :: xs
        | conv ((n, b, "TIME", 1, e, f), (Ty.Text x) :: xs, 0) = (Ty.Time x) :: xs
        | conv ((n, b, "TIME", d, e, 1), (Ty.Text x) :: xs, 0) = (Ty.Time x) :: xs
        | conv ((n, b, "TIME", d, e, 2), (Ty.Text x) :: xs, 0) = (Ty.Time x) :: xs
        | conv ((n, b, "TIME", d, e, f), (Ty.Text x) :: xs, 0) = (Ty.Option (Ty.Time x)) :: xs
        | conv ((n, b, c, d, e, f), x :: xs, k) = x :: conv ((n, b, c, d, e, f), xs, k - 1)
        | conv (_, _, _) = raise Fail "invalid input"
    in
      conv ((n, b, c, d, e, f), l, n)
    end

  (* convert a list of lists *)
  fun correctTypes ((n, b, c, d, e, f), []) = []
    | correctTypes ((n, b, c, d, e, f), l :: ls) =
        (convert ((n, b, c, d, e, f), l)) :: correctTypes ((n, b, c, d, e, f), ls)

  (* convert a list of lists for multiple attributes *)
  fun totalCorrectTypes ([], l) = l
    | totalCorrectTypes ((n, b, c, d, e, f) :: rs, l) =
        totalCorrectTypes (rs, correctTypes ((n, b, c, d, e, f), l))

  fun run (db, cmd) =
    let
      val _ = OS.Process.system ("sqlite3 " ^ db ^ " " ^ cmd ^ " > result.txt")
      val rows = toText (Scan.readlist "result.txt")
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
