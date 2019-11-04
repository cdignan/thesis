structure Scan : sig

  val readlist : string -> string list list
  val scan     : string -> Token.token list

end = struct

  (* function to separate one row into list of elements *)
  fun separateRow (row : string, delimiters : char list) =
    let
      fun process (strs, currChars, []) = strs@[implode currChars]
        | process (strs, currChars, #"\n"::[]) = strs@[implode currChars]
        | process (strs, [], c::nextChars) =
            (case List.find (fn x => c = x) delimiters
              of SOME x => process (strs, [], nextChars)
               | NONE => process (strs, [c], nextChars))
        | process (strs, currChars, c::nextChars) =
            (case List.find (fn x => c = x) delimiters
              of SOME x => process (strs@[implode currChars], [], nextChars)
               | NONE => process (strs, currChars@[c], nextChars))
    in
      process ([], [], explode row)
    end

  (* same function as above, except for rows in table_info output instead of cmd *)
  fun separateList (row : string, delimiters : char list) =
    let
      fun process (strs, currChars, []) = strs@[implode currChars]
        | process (strs, currChars, #"\n"::[]) = strs@[implode currChars]
        | process (strs, [], c::nextChars) =
            (case List.find (fn x => c = x) delimiters
              of SOME x => process (strs@[""], [], nextChars) (* this line is the only difference *)
               | NONE => process (strs, [c], nextChars))
        | process (strs, currChars, c::nextChars) =
            (case List.find (fn x => c = x) delimiters
              of SOME x => process (strs@[implode currChars], [], nextChars)
               | NONE => process (strs, currChars@[c], nextChars))
    in
      process ([], [], explode row)
    end

  (* reads in query output and converts to string list list *)
  fun readlist infile =
    let
      val ins = TextIO.openIn infile
      fun loop ins =
        case TextIO.inputLine ins
          of SOME row => separateList (row, [#"|"]) :: loop ins
           | NONE => []
    in
      loop ins before TextIO.closeIn ins
    end

  (* note: if user has any relations or attributes named "select", "where", etc.
     this will cause problems *)
  fun scan cmd =
    let
      val stringList = separateRow (cmd, [#" ", #",", #"'", #"\n", #"\t"])
      fun toToken [] = []
        | toToken (s::ss) =
            (case s
              of "SELECT" => Token.Select :: toToken ss
               | "select" => Token.Select :: toToken ss
               | "FROM" => Token.From :: toToken ss
               | "from" => Token.From :: toToken ss
               | "NATURAL" =>
                   (case ss
                     of "JOIN"::sss => Token.NatJoin :: toToken sss
                      | _ => raise Fail "expected JOIN after NATURAL")
               | "natural" =>
                   (case ss
                     of "join"::sss => Token.NatJoin :: toToken sss
                      | _ => raise Fail "expected JOIN after NATURAL")
               | "WHERE" => []
               | "where" => []
               | _ => Token.String s :: toToken ss)
    in
      toToken stringList
    end

(* TODO: fix to support if two tables in select statement have the same attribute name *)

end
