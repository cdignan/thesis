structure Typecheck : sig

  val typeof : string * AST.queryTerm -> Ty.ty

end = struct

  (* takes in table (string), returns record type *)
  fun getSchema (db : string, table : string) =
    let
      val _ = OS.Process.system ("sqlite3 " ^ db ^ " 'PRAGMA table_info(" ^ table ^ ")' > " ^ table ^ ".txt")
      val schema = Scan.readlist (table ^ ".txt")
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

  (* takes in list of records, returns record *)
  fun concatRecords ((Ty.Record list1)::[]) = Ty.Record list1
    | concatRecords ((Ty.Record list1)::(Ty.Record list2)::records) = concatRecords ((Ty.Record (list1@list2))::records)
    | concatRecords _ = raise Fail "improper input to concatRecords"

  (* take in string and record, return SOME (record with single item) if found, otherwise NONE *)
  fun getElement (str, Ty.Record list1) =
        (case List.filter (fn (x, y) => x = str) list1
          of [] => NONE
           | list2 => SOME (Ty.Record list2))
    | getElement (_, _) = raise Fail "getElement must take record type"

  (* take in string and list of records, return same as getElement *)
  fun getElementFromList (str, []) = NONE
    | getElementFromList (str, (Ty.Record list1)::records) =
        (case getElement (str, Ty.Record list1)
          of SOME record => SOME record
           | NONE => getElementFromList (str, records))
    | getElementFromList (_, _) = raise Fail "improper input to getElementFromList"

  (* returns record *)
  fun typeof (db, select) =
    let
      fun listOfRecords (db, AST.Select ("*"::[], tables)) = List.map getSchema (List.map (fn x => (db, x)) tables)
        | listOfRecords (db, AST.Select ("*"::columns, tables)) =
            (concatRecords (listOfRecords (db, AST.Select ("*"::[], tables))))::(listOfRecords (db, AST.Select (columns, tables)))
        | listOfRecords (db, AST.Select (column::[], tables)) =
            (case getElementFromList (column, (listOfRecords (db, AST.Select ("*"::[], tables))))
              of SOME record => record::[]
               | NONE => raise Fail "requested column doesn't exist")
        | listOfRecords (db, AST.Select (column::columns, tables)) =
            (concatRecords (listOfRecords (db, AST.Select (column::[], tables))))::(listOfRecords (db, AST.Select (columns, tables)))
        | listOfRecords (_, _) = raise Fail "must input select statement to typeof"
    in
      concatRecords (listOfRecords (db, select))
    end

end
