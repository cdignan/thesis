structure Scan : sig

  val separateRow : string * char list -> string list
  val scan        : string -> Token.token list

end = struct

  (* function to separate one row into list of elements *)
  (* can be used both for query and in reading a file (see typecheck.sml readfile function) *)
  fun separateRow (row, delimiters) =
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
