structure Test = struct

  (* type Test.run (db, cmd) in the SML repl *)

  fun run (db, cmd) =
    let
      val _ = OS.Process.system ("sqlite3 " ^ db ^ " " ^ cmd ^ " > result.txt")
      val rows = Scan.readlist "result.txt"
      val scan = Scan.scan cmd
      val parse = Parse.parse (db, scan)
    in
      (Eval.eval parse, rows)
    end

end
