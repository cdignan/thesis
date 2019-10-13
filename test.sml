structure Test = struct

  (* type Test.run (db, cmd) in the SML repl *)

  (* function to separate one row into list of elements *)
  fun separateRow (row : string) =
    let
      fun process (strs, currChars, []) = strs@[implode currChars]
        | process (strs, currChars, #"\n"::[]) = strs@[implode currChars]
        | process (strs, currChars, #"|"::nextChars) = process (strs@[implode currChars], [], nextChars)
        | process (strs, currChars, c::nextChars) = process (strs, currChars@[c], nextChars)
    in
      process ([], [], explode row)
    end

  (* reads in query output and converts to list of strings *)
  fun readlist (infile : string) =
    let
      val ins = TextIO.openIn infile
      fun loop ins =
        case TextIO.inputLine ins
         of SOME row => separateRow row :: loop ins
          | NONE => []
    in
      loop ins before TextIO.closeIn ins
    end

  fun run (db, cmd) =
    let
      val _ = OS.Process.system ("sqlite3 " ^ db ^ " " ^ cmd ^ "> result.txt")
    in
      readlist "result.txt"
    end

end
