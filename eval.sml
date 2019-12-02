structure Eval : sig

  val eval : AST.term -> AST.term

end = struct

  fun setCID (e : {cid: int, attribute: string, ty: AST.types, notnull: bool, dflt_val: string,
                  primary_key: AST.pk option, foreign_key: AST.fk option, unique: bool, tables: string list}, n) =
        {cid = n, attribute = #attribute e, ty = #ty e, notnull = #notnull e, dflt_val = #dflt_val e,
        primary_key = #primary_key e, foreign_key = #foreign_key e, unique = #unique e, tables = #tables e}

  fun setAttr (e : {cid: int, attribute: string, ty: AST.types, notnull: bool, dflt_val: string,
                  primary_key: AST.pk option, foreign_key: AST.fk option, unique: bool, tables: string list}, attr) =
        {cid = #cid e, attribute = attr, ty = #ty e, notnull = #notnull e, dflt_val = #dflt_val e,
        primary_key = #primary_key e, foreign_key = #foreign_key e, unique = #unique e, tables = #tables e}

  fun setTy (e : {cid: int, attribute: string, ty: AST.types, notnull: bool, dflt_val: string,
                  primary_key: AST.pk option, foreign_key: AST.fk option, unique: bool, tables: string list}, typ) =
        {cid = #cid e, attribute = #attribute e, ty = typ, notnull = #notnull e, dflt_val = #dflt_val e,
        primary_key = #primary_key e, foreign_key = #foreign_key e, unique = #unique e, tables = #tables e}

  fun setNull (e : {cid: int, attribute: string, ty: AST.types, notnull: bool, dflt_val: string,
                  primary_key: AST.pk option, foreign_key: AST.fk option, unique: bool, tables: string list}, b) =
        {cid = #cid e, attribute = #attribute e, ty = #ty e, notnull = b, dflt_val = #dflt_val e,
        primary_key = #primary_key e, foreign_key = #foreign_key e, unique = #unique e, tables = #tables e}

  fun setDflt (e : {cid: int, attribute: string, ty: AST.types, notnull: bool, dflt_val: string,
                  primary_key: AST.pk option, foreign_key: AST.fk option, unique: bool, tables: string list}, dflt) =
        {cid = #cid e, attribute = #attribute e, ty = #ty e, notnull = #notnull e, dflt_val = dflt,
        primary_key = #primary_key e, foreign_key = #foreign_key e, unique = #unique e, tables = #tables e}

  fun setPK (e : {cid: int, attribute: string, ty: AST.types, notnull: bool, dflt_val: string,
                  primary_key: AST.pk option, foreign_key: AST.fk option, unique: bool, tables: string list}, prim) =
        {cid = #cid e, attribute = #attribute e, ty = #ty e, notnull = #notnull e, dflt_val = #dflt_val e,
        primary_key = prim, foreign_key = #foreign_key e, unique = #unique e, tables = #tables e}

  fun setFK (e : {cid: int, attribute: string, ty: AST.types, notnull: bool, dflt_val: string,
                  primary_key: AST.pk option, foreign_key: AST.fk option, unique: bool, tables: string list}, for) =
        {cid = #cid e, attribute = #attribute e, ty = #ty e, notnull = #notnull e, dflt_val = #dflt_val e,
        primary_key = #primary_key e, foreign_key = for, unique = #unique e, tables = #tables e}

  fun setUnique (e : {cid: int, attribute: string, ty: AST.types, notnull: bool, dflt_val: string,
                     primary_key: AST.pk option, foreign_key: AST.fk option, unique: bool, tables: string list}, uniq) =
        {cid = #cid e, attribute = #attribute e, ty = #ty e, notnull = #notnull e, dflt_val = #dflt_val e,
        primary_key = #primary_key e, foreign_key = #foreign_key e, unique = uniq, tables = #tables e}

  fun setTables (e : {cid: int, attribute: string, ty: AST.types, notnull: bool, dflt_val: string,
                  primary_key: AST.pk option, foreign_key: AST.fk option, unique: bool, tables: string list}, tab) =
        {cid = #cid e, attribute = #attribute e, ty = #ty e, notnull = #notnull e, dflt_val = #dflt_val e,
        primary_key = #primary_key e, foreign_key = #foreign_key e, unique = #unique e, tables = tab}

  (* if schema, after a natural join, has two of the same attribute, this
     function will remove one of them *)
  fun removeDuplicates ([], l) = l
    | removeDuplicates (x::xs : {cid: int, attribute: string, ty: AST.types, notnull: bool, dflt_val: string,
                                primary_key: AST.pk option, foreign_key: AST.fk option, unique: bool, tables: string list} list, l) =
        (case List.filter (fn x' => (#attribute x) = (#attribute x')) l
          of x'' :: [] => setTables (x, (#tables x)@(#tables x'')) ::
                          removeDuplicates (xs, List.filter (fn x' => (#attribute x) <> (#attribute x')) l)
           | [] => x :: removeDuplicates (xs, l)
           | _ => raise Fail "currently don't support when a relation has multiple columns of same name in natural join")

  (* after all of the joining between tables and everything, this function changes the
     column ids and pks to be accurate in the result *)
  fun resetcid ([], _) = []
    | resetcid ((x :: xs) : {cid: int, attribute: string, ty: AST.types, notnull: bool, dflt_val: string,
                            primary_key: AST.pk option, foreign_key: AST.fk option, unique: bool, tables: string list} list, n) =
        (case #primary_key x
          of SOME (AST.PK _) => setFK (setPK (setNull (setCID (x, n), true), NONE), NONE) :: resetcid (xs, n + 1)
           | _ => setFK (setPK (setCID (x, n), NONE), NONE) :: resetcid (xs, n + 1))

  fun union ([], []) = []
    | union (x1 :: l1 : {cid: int, attribute: string, ty: AST.types, notnull: bool, dflt_val: string,
                        primary_key: AST.pk option, foreign_key: AST.fk option, unique: bool, tables: string list} list,
             x2 :: l2 : {cid: int, attribute: string, ty: AST.types, notnull: bool, dflt_val: string,
                        primary_key: AST.pk option, foreign_key: AST.fk option, unique: bool, tables: string list} list) =
        if (#attribute x1) = (#attribute x2) andalso (#ty x1) = (#ty x2)
        then setTables (setFK (setPK (setDflt (setNull (x1, false), ""), NONE), NONE), (#tables x1)@(#tables x2)) :: union (l1, l2)
        else raise Fail "invalid union"
    | union (_, _) = raise Fail "union must be same number of attrs"

  (* evaluate each term *)
  fun eval (AST.Relation ls) = AST.Relation ls
    | eval (AST.LeftNatJoin (rel1, rel2)) =
        (case (eval rel1, eval rel2)
          of (AST.Relation l1, AST.Relation l2) =>
               AST.Relation (resetcid (removeDuplicates (List.map (fn x => setPK (setNull (x, false), NONE)) l1, l2), 0))
           | (_, _) => raise Fail "eval error - left natural join")
    | eval (AST.NatJoin (rel1, rel2)) =
        (case (eval rel1, eval rel2)
          of (AST.Relation l1, AST.Relation l2) => AST.Relation (resetcid (removeDuplicates (l1, l2), 0))
           | (_, _) => raise Fail "eval error - natural join")
    | eval (AST.Proj (("*", "*", _)::[], rel)) = eval rel
    | eval (AST.Proj (("*", "*", _)::strs, rel)) = eval (AST.NatJoin (eval rel, AST.Proj (strs, rel))) (* TODO: this could cause the schema and relation to not match *)
    | eval (AST.Proj ((str1, str2, _)::[], rel)) =
        (case eval rel
          of AST.Relation ls =>
               if List.exists (fn x => x = #".") (explode str1)
               then (let
                      val tableAndAttr = Scan.separateRow (str1, [#"."])
                      val table = List.nth (tableAndAttr, 0)
                      val attr = List.nth (tableAndAttr, 1)
                    in
                      AST.Relation (List.map (fn x => setAttr (x, str2))
                                             (resetcid ((List.filter (fn e : {cid: int, attribute: string, ty: AST.types, notnull: bool, dflt_val: string,
                                                                             primary_key: AST.pk option, foreign_key: AST.fk option, unique: bool, tables: string list} =>
                                                                       attr = (#attribute e) andalso
                                                                       List.exists (fn x => x = table) (#tables e)) ls), 0)))
                    end)
               else AST.Relation (List.map (fn x => setAttr (x, str2))
                                           (resetcid ((List.filter (fn x => (#attribute x) = str1) ls), 0)))
           | _ => raise Fail "eval error - projection")
    | eval (AST.Proj (str::strs, rel)) = eval (AST.NatJoin (AST.Proj (str::[], rel), AST.Proj (strs, rel))) (* TODO: this could cause the shema and relation to not match *)
    | eval (AST.Union (rel1, rel2)) =
        (case (eval rel1, eval rel2)
          of (AST.Relation l1, AST.Relation l2) => AST.Relation (union (l1, l2))
           | (_, _) => raise Fail "eval error - union")
    | eval _ = raise Fail "improper input to eval"

end
