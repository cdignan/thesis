structure Scan = struct

  (* function to separate one row into list of elements *)
  fun separateRow (row : string, delimiters : char list) =
    let
      fun process (strs, currChars, []) = strs@[implode currChars]
        | process (strs, currChars, #"\n"::[]) = strs@[implode currChars]
        | process (strs, [], c::nextChars) =
            (case List.find (fn x => c = x) delimiters
              of SOME x => process (strs@[""], [], nextChars)
               | NONE => process (strs, [c], nextChars))
        | process (strs, currChars, c::nextChars) =
            (case List.find (fn x => c = x) delimiters
              of SOME x => process (strs@[implode currChars], [], nextChars)
               | NONE => process (strs, currChars@[c], nextChars))
    in
      process ([], [], explode row)
    end

  (* reads in query output and converts to string list list *)
  fun readlist (infile : string) =
    let
      val ins = TextIO.openIn infile
      fun loop ins =
        case TextIO.inputLine ins
          of SOME row => separateRow (row, [#"|"]) :: loop ins
           | NONE => []
    in
      loop ins before TextIO.closeIn ins
    end

  (* takes in table (string), returns record type *)
  fun getSchema (db : string, table : string) =
    let
      val _ = OS.Process.system ("sqlite3 " ^ db ^ " 'PRAGMA table_info(" ^ table ^ ")' > " ^ table ^ ".txt")
      val schema = readlist (table ^ ".txt")
      fun loop [] = []
        | loop (l::ls) =
            case List.nth (l, 2)
              of "INTEGER" => (List.nth (l, 1), Ty.Int) :: loop ls
               | "TEXT" => (List.nth (l, 1), Ty.Text) :: loop ls
               | "DATE" => (List.nth (l, 1), Ty.Date) :: loop ls
               | "TIME" => (List.nth (l, 1), Ty.Time) :: loop ls
               | _ => (List.nth (l, 1), Ty.Unit) :: loop ls
    in
      Ty.Record (loop schema)
    end

  fun scan (cmd : string) =
    let
      val stringList = separateRow (cmd, [#" ", #",", #"'", #"\n", #"\t"])
      fun toToken [] = []
        | toToken (s::ss) =
            (case explode s
              of [#"S", #"E", #"L", #"E", #"C", #"T"] => Token.Select :: toToken ss
               | [#"s", #"e", #"l", #"e", #"c", #"t"] => Token.Select :: toToken ss
               | [#"F", #"R", #"O", #"M"] => Token.From :: toToken ss
               | [#"f", #"r", #"o", #"m"] => Token.From :: toToken ss
               | _ => Token.String s :: toToken ss)
    in
      toToken stringList
    end

  fun parse tokens =
    let
      fun toList (strs, strs2, "from", []) = AST.Select (strs, strs2)
        | toList (strs, strs2, "from", Token.String str::ts) = toList (strs, strs2@[str], "from", ts)
        | toList (strs, [], "select", Token.From :: ts) = toList (strs, [], "from", ts)
        | toList (strs, [], "select", Token.String str :: ts) = toList (strs@[str], [], "select", ts)
        | toList ([], [], mode, Token.Select :: ts) = toList ([], [], "select", ts)
        | toList (_, _, _, _) = raise Fail "improper query format"
    in
      toList ([], [], "", tokens)
    end

(* TODO: use AST.Select term to figure out which tables to call getSchema on,
and which columns from those tables to include in a new record type.
then use the schema to go through result from readlist and convert to
a record of terms, with the label being the label in the schema - the column name *)

end
