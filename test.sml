structure Test = struct

  (* type Test.run (db, cmd) in the SML repl *)

  (* helper function to remove newline char when reading in the
     rows of the query output into the list of strings *)
  fun remCharR (c, s) =
    let
      fun rem [] = []
        | rem (c'::cs) =
            if c = c'
            then rem cs
            else c'::rem cs
    in
      implode (rem (explode s))
    end

  (* reads in query output and converts to list of strings *)
  fun readlist (infile : string) =
    let
      val ins = TextIO.openIn infile
      fun loop ins =
        case TextIO.inputLine ins
         of SOME line => remCharR (#"\n", line) :: loop ins
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
