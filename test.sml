structure Test = struct

  (* type Test.run (db, cmd) in the SML repl *)

  (* takes in a string list list, returns a Ty.ty list list
     everything will be Ty.Text but that will be corrected in covert *)
  fun toText [] = []
    | toText (l :: ls) = (List.map (fn x => Ty.Text x) l) :: toText ls

  (* take in one attribute with it's info, and one list of Ty.Text terms
     return corrected list of Ty.ty terms *)
  fun convert (((a, n), b, c, d, e, f, g), l) =
    let
      fun conv ((_, _, (_, "INTEGER"), (_, 1), _, _, _), (Ty.Text x) :: xs, 0) =
            (case Int.fromString x
              of SOME x' => (Ty.Int x') :: xs
               | NONE => raise Fail "expected int")
        | conv ((_, _, (_, "INTEGER"), _, _, (_, 1), _), (Ty.Text x) :: xs, 0) =
            (case Int.fromString x
              of SOME x' => (Ty.Int x') :: xs
               | NONE => raise Fail "expected int")
        | conv ((_, _, (_, "INTEGER"), _, _, (_, 2), _), (Ty.Text x) :: xs, 0) =
            (case Int.fromString x
              of SOME x' => (Ty.Int x') :: xs
               | NONE => raise Fail "expected int")
        | conv ((_, _, (_, "INTEGER"), _, _, _, _), (Ty.Text x) :: xs, 0) =
            (case Int.fromString x
              of SOME x' => (Ty.Option (Ty.Int x')) :: xs
               | NONE => raise Fail "expected int")
        | conv ((_, _, (_, "TEXT"), (_, 1), _, _, _), x :: xs, 0) = x :: xs
        | conv ((_, _, (_, "TEXT"), _, _, (_, 1), _), x :: xs, 0) = x :: xs
        | conv ((_, _, (_, "TEXT"), _, _, (_, 2), _), x :: xs, 0) = x :: xs
        | conv ((_, _, (_, "TEXT"), _, _, _, _), x :: xs, 0) = (Ty.Option x) :: xs
        | conv ((_, _, (_, "DATE"), (_, 1), _, _, _), (Ty.Text x) :: xs, 0) = (Ty.Date x) :: xs
        | conv ((_, _, (_, "DATE"), _, _, (_, 1), _), (Ty.Text x) :: xs, 0) = (Ty.Date x) :: xs
        | conv ((_, _, (_, "DATE"), _, _, (_, 2), _), (Ty.Text x) :: xs, 0) = (Ty.Date x) :: xs
        | conv ((_, _, (_, "DATE"), _, _, _, _), (Ty.Text x) :: xs, 0) = (Ty.Option (Ty.Date x)) :: xs
        | conv ((_, _, (_, "TIME"), (_, 1), _, _, _), (Ty.Text x) :: xs, 0) = (Ty.Time x) :: xs
        | conv ((_, _, (_, "TIME"), _, _, (_, 1), _), (Ty.Text x) :: xs, 0) = (Ty.Time x) :: xs
        | conv ((_, _, (_, "TIME"), _, _, (_, 2), _), (Ty.Text x) :: xs, 0) = (Ty.Time x) :: xs
        | conv ((_, _, (_, "TIME"), _, _, _, _), (Ty.Text x) :: xs, 0) = (Ty.Option (Ty.Time x)) :: xs
        | conv (t, x :: xs, k) = x :: conv (t, xs, k - 1)
        | conv (_, _, _) = raise Fail "invalid input"
    in
      conv (((a, n), b, c, d, e, f, g), l, n)
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
      val _ = OS.Process.system ("sqlite3 " ^ db ^ " " ^ "\"" ^ cmd ^ "\"" ^ " > result.txt")
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
