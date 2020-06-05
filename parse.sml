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
      fun getPK ls = (case List.filter (fn l => List.nth (l, 3) = "pk") ls
                       of idx :: _ => let
                                        val _ = OS.Process.system ("sqlite3 -csv -noheader " ^ db ^ " 'PRAGMA index_info("
                                                                  ^ List.nth (idx, 1) ^ ")' > pk_info.csv")
                                        val indexInfo = Scan.readlist "pk_info.csv"
                                      in
                                        List.map (fn x => List.nth (x, 2)) indexInfo
                                      end
                        | [] => (case List.filter (fn l => List.nth (l, 5) = "1") schema
                                  of i :: _ => [List.nth (i, 1)]
                                   | [] => []))
      fun getFK [] = []
        | getFK ([a, b, c, d, e, f, g, h] :: ls : string list list) =
            (case List.partition (fn x => List.nth (x, 0) = a) ([a, b, c, d, e, f, g, h] :: ls)
              of (y, ys) => (List.map (fn l => {table = List.nth (l, 2), from = List.nth (l, 3), to = List.nth (l, 4)}) y) :: (getFK ys))
        | getFK _ = raise Fail "invalid foreign key list"
      fun listEqual ([], []) = true
        | listEqual (_, []) = false
        | listEqual ([], _) = false
        | listEqual (x :: xs, ys) = if (List.exists (fn z => z = x) ys)
                                    then listEqual (List.filter (fn z => z <> x) xs, List.filter (fn z => z <> x) ys)
                                    else false
      fun equalToAny (_, []) = false
        | equalToAny (p, u :: us) = if listEqual (p, u) then true else equalToAny (p, us)
      fun possiblyAppendPK (p, us) = if equalToAny (p, us) orelse p = []
                                     then us
                                     else p :: us
      fun getUnique [] = []
        | getUnique (l :: ls) =
            if List.nth (l, 3) = "u"
            then (let
                    val _ = OS.Process.system ("sqlite3 -csv -noheader " ^ db ^ " 'PRAGMA index_info("
                                              ^ List.nth (l, 1) ^ ")' > unique_info.csv")
                    val indexInfo = Scan.readlist "unique_info.csv"
                  in
                    (List.map (fn x => List.nth (x, 2)) indexInfo) :: getUnique ls
                  end)
            else getUnique ls
      fun loop [] = []
        | loop (l::ls) =
            {attribute = List.nth (l, 1),
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
            tables = [table]} :: loop ls
    in
      AST.Relation (loop schema, possiblyAppendPK (getPK indexList, getUnique indexList), SOME (getPK indexList), SOME (getFK fkey))
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

  fun cartProd (db, rel, (Token.String str) :: []) =
        AST.CartProd (rel, getSchema (db, str), [])
    | cartProd (db, rel, (Token.String str) :: Token.Union :: _) =
        AST.CartProd (rel, getSchema (db, str), [])
    | cartProd (db, rel, (Token.String str) :: Token.Where :: toks) =
        AST.CartProd (rel, getSchema (db, str), afterWhere toks)
    | cartProd (_, _, _) = raise Fail "cartProd - invalid token list"
  and afterWhere ((Token.String str1) :: Token.Equals :: (Token.String str2) :: Token.And :: toks) =
        (str1, str2) :: afterWhere toks
    | afterWhere ((Token.String str1) :: Token.Equals :: (Token.String str2) :: Token.Union :: _) = [(str1, str2)]
    | afterWhere ((Token.String str1) :: Token.Equals :: (Token.String str2) :: []) = [(str1, str2)]
    | afterWhere _ = []

  (* evaluates to natural join term if there is a natural join expression after FROM *)
  fun join (db, (Token.String str) :: []) = getSchema (db, str)
    | join (db, (Token.String str) :: Token.Where :: _) = getSchema (db, str)
    | join (db, (Token.String str) :: Token.Union :: _) = getSchema (db, str)
    | join (db, (Token.String str) :: Token.CartProd :: toks) =
        cartProd (db, getSchema (db, str), toks)
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
