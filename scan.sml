structure Scan : sig

  val separateRow : string * char list -> string list
  val readlist    : string -> string list list
  val scan        : string -> Token.token list

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
          of SOME row => separateList (row, [#","]) :: loop ins
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
        | toToken ("SELECT" :: "DISTINCT" :: ss) = Token.SelectDistinct :: toToken ss
        | toToken ("select" :: "distinct" :: ss) = Token.SelectDistinct :: toToken ss
        | toToken ("SELECT" :: ss) = Token.SelectAll :: toToken ss
        | toToken ("select" :: ss) = Token.SelectAll :: toToken ss
        | toToken ("AS" :: ss) = Token.As :: toToken ss
        | toToken ("as" :: ss) = Token.As :: toToken ss
        | toToken ("FROM" :: ss) = Token.From :: toTokenAfterFrom ss
        | toToken ("from" :: ss) = Token.From :: toTokenAfterFrom ss
        | toToken (s :: ss) = (Token.String s) :: toToken ss
      and toTokenAfterFrom [] = []
        | toTokenAfterFrom ("UNION" :: ss) = Token.Union :: toToken ss
        | toTokenAfterFrom ("union" :: ss) = Token.Union :: toToken ss
        | toTokenAfterFrom ("LEFT" :: "NATURAL" :: "JOIN" :: ss) = Token.LeftNatJoin :: toTokenAfterFrom ss
        | toTokenAfterFrom ("left" :: "natural" :: "join" :: ss) = Token.LeftNatJoin :: toTokenAfterFrom ss
        | toTokenAfterFrom ("NATURAL" :: "JOIN" :: ss) = Token.NatJoin :: toTokenAfterFrom ss
        | toTokenAfterFrom ("natural" :: "join" :: ss) = Token.NatJoin :: toTokenAfterFrom ss
        | toTokenAfterFrom ("LEFT" :: "JOIN" :: ss) = Token.LeftOuterJoin :: toTokenAfterFrom ss
        | toTokenAfterFrom ("left" :: "join" :: ss) = Token.LeftOuterJoin :: toTokenAfterFrom ss
        | toTokenAfterFrom ("JOIN" :: ss) = Token.InnerJoin :: toTokenAfterFrom ss
        | toTokenAfterFrom ("join" :: ss) = Token.InnerJoin :: toTokenAfterFrom ss
        | toTokenAfterFrom ("WHERE" :: ss) = toTokenAfterWhere ss
        | toTokenAfterFrom ("where" :: ss) = toTokenAfterWhere ss
        | toTokenAfterFrom (s :: ss) = (Token.String s) :: toTokenAfterFrom ss
      and toTokenAfterWhere ("UNION" :: ss) = Token.Union :: toToken ss
        | toTokenAfterWhere ("union" :: ss) = Token.Union :: toToken ss
        | toTokenAfterWhere [] = []
        | toTokenAfterWhere (_ :: ss) = toTokenAfterWhere ss
    in
      toToken stringList
    end

end
