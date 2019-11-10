structure Parse : sig

  val parse : string * (Token.token list) -> AST.term

end = struct

  (* this function gets the schema of one table *)
  fun getSchema (db : string, table : string) =
    let
      val _ = OS.Process.system ("sqlite3 -noheader " ^ db ^ " 'PRAGMA table_info(" ^ table ^ ")' > " ^ table ^ ".txt")
      val schema = Scan.readlist (table ^ ".txt")
      val _ = OS.Process.system ("sqlite3 -noheader " ^ db ^ " 'PRAGMA foreign_key_list(" ^ table ^ ")' > fkey.txt")
      val fkey = Scan.readlist "fkey.txt"
      fun loop ([], fkey) = []
        | loop (l::ls, fkey) =
            (("Column ID",
            (case Int.fromString (List.nth (l, 0))
              of SOME n => n
               | NONE => raise Fail "invalid column id")),
            ("Attribute", List.nth (l, 1)),
            ("Type", List.nth (l, 2)),
            ("Not Null",
            (case Int.fromString (List.nth (l, 3))
              of SOME 0 => false
               | SOME 1 => true
               | _ => raise Fail "invalid not null constaint")),
            ("Default Value", List.nth (l, 4)),
            ("Primary Key",
            (case Int.fromString (List.nth (l, 5))
              of SOME 0 => SOME AST.NotPK
               | SOME 1 => SOME AST.PK
               | SOME 2 =>
                   (case List.filter (fn sublist => List.nth (sublist, 3) = List.nth (l, 1)) fkey
                     of [_, b, c, d, e, f, g, h]  :: [] =>
                          SOME (AST.FK (("seq",
                                        (case Int.fromString b
                                          of SOME n => n
                                           | NONE => raise Fail "invalid seq #")),
                                        ("table", c),
                                        ("from", d),
                                        ("to", e),
                                        ("on_update", f),
                                        ("on_delete", g),
                                        ("match", h)))
                      | _ => raise Fail "invalid foreign key")
               | _ => raise Fail "invalid primary key constraint")),
            ("Tables", [table])) :: loop (ls, fkey)
    in
      AST.Relation (loop (schema, fkey))
    end

  (* take in tokens, return list of strings (attributes)
     note that this function takes in all tokens, but in essence can be considered to only be
     looking at the token strings - once it gets to Token.From, it just discards the rest *)
  fun toAttributes (Token.From :: toks) = []
    | toAttributes ((Token.String str) :: toks) = str :: (toAttributes toks)
    | toAttributes _ = raise Fail "only attributes can come after SELECT clause"

  (* evaluates to cartesian product term if there is a list of tables after FROM
     evaluates to natural join term if there is a natural join expression after FROM *)
  fun join (db, (Token.String str) :: []) = getSchema (db, str)
    | join (db, (Token.String str) :: Token.NatJoin :: toks) =
        AST.NatJoin ((getSchema (db, str)), join (db, toks))
    | join (db, (Token.String str1) :: (Token.String str2) :: toks) =
        AST.CartProd (getSchema (db, str1), join (db, (Token.String str2) :: toks))
    | join (_, _) = raise Fail "improper format after FROM clause"

  (* takes in token list, returns term in relational algebra using above helper functions *)
  fun parse (db, Token.Select :: toks) = AST.Proj (toAttributes toks, parse (db, toks))
    | parse (db, Token.From :: toks) = join (db, toks)
    | parse (db, (Token.String _) :: toks) = parse (db, toks)
    | parse (_, _) = raise Fail "parse error"

end
