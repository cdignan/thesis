structure Test = struct

  (* type Test.run (db, cmd) in the SML repl *)

  fun run db = OS.Process.system ("sqlite3 " ^ db)

end
