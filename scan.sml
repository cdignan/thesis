structure Scan : sig

  val readlist : string -> string list list
  val scan     : string -> Token.token list

end = struct

  (* function to separate one row into list of elements *)
  (* can be used both for query and in reading a file (see readfile function) *)
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

  (* reads in query output and converts to string list list *)
  fun readlist infile =
    let
      val ins = TextIO.openIn infile
      fun loop ins =
        case TextIO.inputLine ins
          of SOME row => separateRow (row, [#"|"]) :: loop ins
           | NONE => []
    in
      loop ins before TextIO.closeIn ins
    end

  fun scan cmd =
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

(* TODO: use the schema to go through result from readlist and convert to
a record of terms, with the label being the label in the schema - the column name *)

end
