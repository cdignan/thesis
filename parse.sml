structure Parse : sig

  val parse : string * (Token.token list) -> AST.term

end = struct

  (* this function gets the schema of one table *)
  fun getSchema (db : string, table : string) =
    let
      val _ = OS.Process.system ("sqlite3 -csv -noheader " ^ db ^ " 'PRAGMA table_info(" ^ table ^ ")' > " ^ table ^ ".csv")
      val schema = Scan.readlist (table ^ ".csv")
      val _ = OS.Process.system ("sqlite3 -csv -noheader " ^ db ^ " 'PRAGMA foreign_key_list(" ^ table ^ ")' > fkey.csv")
      val fkey = Scan.readlist "fkey.csv"
      val _ = OS.Process.system ("sqlite3 -csv -noheader " ^ db ^ " 'PRAGMA index_list(" ^ table ^ ")' > index_list.csv")
      val indexList = Scan.readlist "index_list.csv"
      fun getUnique ([], _) = false
        | getUnique (l::ls, attr) =
            if List.nth (l, 3) = "u"
            then (let
                    val _ = OS.Process.system ("sqlite3 -csv -noheader " ^ db ^ " 'PRAGMA index_info("
                                              ^ List.nth (l, 1) ^ ")' > index_info.csv")
                    val indexInfo = Scan.readlist "index_info.csv"
                  in
                    (case List.filter (fn x => List.nth (x, 2) = attr) indexInfo
                      of [] => getUnique (ls, attr)
                       | _ => true)
                  end)
            else getUnique (ls, attr)
      fun loop ([], fkey) = []
        | loop (l::ls, fkey) =
            {cid =
            (case Int.fromString (List.nth (l, 0))
              of SOME n => n
               | NONE => raise Fail "invalid column id"),
            attribute = List.nth (l, 1),
            ty =
            (case List.nth (l, 2)
              of "INTEGER" => AST.Int
               | "TEXT" => AST.Text
               | "DATE" => AST.Date
               | "TIME" => AST.Time
               | _ => raise Fail "invalid type"),
            notnull =
            (case Int.fromString (List.nth (l, 3))
              of SOME 0 => false
               | SOME 1 => true
               | _ => raise Fail "invalid not null constaint"),
            dflt_val = List.nth (l, 4),
            primary_key =
            (case Int.fromString (List.nth (l, 5))
              of SOME 0 => SOME AST.NotPK
               | SOME n => SOME (AST.PK n)
               | _ => raise Fail "invalid primary key constraint"),
            foreign_key =
            (case List.filter (fn sublist => List.nth (sublist, 3) = List.nth (l, 1)) fkey
              of [] => SOME AST.NotFK
               | [_, b, c, d, e, f, g, h]  :: [] =>
                   SOME (AST.FK {seq =
                                  (case Int.fromString b
                                    of SOME n => n
                                     | NONE => raise Fail "invalid seq #"),
                                 table = c,
                                 from = d,
                                 to = e,
                                 on_update = f,
                                 on_delete = g,
                                 matc = h})
               | _ => raise Fail "invalid foreign key"),
            unique = getUnique (indexList, List.nth (l, 1)),
            tables = [table]} :: loop (ls, fkey)
    in
      AST.Relation (loop (schema, fkey))
    end

  (* take in tokens, return list of strings (attributes)
     note that this function takes in all tokens, but in essence can be considered to only be
     looking at the token strings - once it gets to Token.From, it just discards the rest *)
  fun toAttributes (Token.From :: toks, _) = []
    | toAttributes ((Token.String str1) :: Token.As :: (Token.String str2) :: toks, false) =
        (str1, str2, false) :: toAttributes (toks, false)
    | toAttributes ((Token.String str1) :: Token.As :: (Token.String str2) :: toks, true) =
        (str1, str2, true) :: toAttributes (toks, true)
    | toAttributes ((Token.String str) :: toks, false) = (str, str, false) :: toAttributes (toks, false)
    | toAttributes ((Token.String str) :: toks, true) = (str, str, true) :: toAttributes (toks, true)
    | toAttributes _ = raise Fail "only attributes can come after SELECT clause"

  (* evaluates to cartesian product term if there is a list of tables after FROM
     evaluates to natural join term if there is a natural join expression after FROM *)
  fun join (db, (Token.String str) :: []) = getSchema (db, str)
    | join (db, (Token.String str) :: Token.Union :: _) = getSchema (db, str)
    | join (db, (Token.String str) :: Token.NatJoin :: toks) =
        AST.NatJoin ((getSchema (db, str)), join (db, toks))
    | join (db, (Token.String str1) :: (Token.String str2) :: toks) =
        AST.CartProd (getSchema (db, str1), join (db, (Token.String str2) :: toks))
    | join (_, _) = raise Fail "improper format after FROM clause"

  fun getUnionList (Token.Union :: toks) = toks
    | getUnionList [] = []
    | getUnionList (tok :: toks) = getUnionList toks

  (* takes in token list, returns term in relational algebra using above helper functions *)
  fun parse (db, Token.SelectAll :: toks) =
        (case getUnionList toks
          of [] => AST.Proj (toAttributes (toks, false), parse (db, toks))
           | toks' => AST.Union (AST.Proj (toAttributes (toks, false), parse (db, toks)), parse (db, toks')))
    | parse (db, Token.SelectDistinct :: toks) =
        (case getUnionList toks
          of [] => AST.Proj (toAttributes (toks, true), parse (db, toks))
           | toks' => AST.Union (AST.Proj (toAttributes (toks, true), parse (db, toks)), parse (db, toks')))
    | parse (db, Token.From :: toks) = join (db, toks)
    | parse (db, (Token.String _) :: toks) = parse (db, toks)
    | parse (db, Token.As :: toks) = parse (db, toks)
    | parse (_, _) = raise Fail "parse error"

end
