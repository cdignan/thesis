structure Eval : sig

  val eval : AST.term -> AST.term

end = struct

  fun setCID (e : {cid: int, attribute: string, ty: AST.types, notnull: bool, dflt_val: string,
                  primary_key: AST.pk option, foreign_key: AST.fk option, tables: string list}, n) =
        {cid = n, attribute = #attribute e, ty = #ty e, notnull = #notnull e, dflt_val = #dflt_val e,
        primary_key = #primary_key e, foreign_key = #foreign_key e, tables = #tables e}

  fun setAttr (e : {cid: int, attribute: string, ty: AST.types, notnull: bool, dflt_val: string,
                  primary_key: AST.pk option, foreign_key: AST.fk option, tables: string list}, attr) =
        {cid = #cid e, attribute = attr, ty = #ty e, notnull = #notnull e, dflt_val = #dflt_val e,
        primary_key = #primary_key e, foreign_key = #foreign_key e, tables = #tables e}

  fun setTy (e : {cid: int, attribute: string, ty: AST.types, notnull: bool, dflt_val: string,
                  primary_key: AST.pk option, foreign_key: AST.fk option, tables: string list}, typ) =
        {cid = #cid e, attribute = #attribute e, ty = typ, notnull = #notnull e, dflt_val = #dflt_val e,
        primary_key = #primary_key e, foreign_key = #foreign_key e, tables = #tables e}

  fun setNull (e : {cid: int, attribute: string, ty: AST.types, notnull: bool, dflt_val: string,
                  primary_key: AST.pk option, foreign_key: AST.fk option, tables: string list}, b) =
        {cid = #cid e, attribute = #attribute e, ty = #ty e, notnull = b, dflt_val = #dflt_val e,
        primary_key = #primary_key e, foreign_key = #foreign_key e, tables = #tables e}

  fun setDflt (e : {cid: int, attribute: string, ty: AST.types, notnull: bool, dflt_val: string,
                  primary_key: AST.pk option, foreign_key: AST.fk option, tables: string list}, dflt) =
        {cid = #cid e, attribute = #attribute e, ty = #ty e, notnull = #notnull e, dflt_val = dflt,
        primary_key = #primary_key e, foreign_key = #foreign_key e, tables = #tables e}

  fun setPK (e : {cid: int, attribute: string, ty: AST.types, notnull: bool, dflt_val: string,
                  primary_key: AST.pk option, foreign_key: AST.fk option, tables: string list}, prim) =
        {cid = #cid e, attribute = #attribute e, ty = #ty e, notnull = #notnull e, dflt_val = #dflt_val e,
        primary_key = prim, foreign_key = #foreign_key e, tables = #tables e}

  fun setFK (e : {cid: int, attribute: string, ty: AST.types, notnull: bool, dflt_val: string,
                  primary_key: AST.pk option, foreign_key: AST.fk option, tables: string list}, for) =
        {cid = #cid e, attribute = #attribute e, ty = #ty e, notnull = #notnull e, dflt_val = #dflt_val e,
        primary_key = #primary_key e, foreign_key = for, tables = #tables e}

  fun setTables (e : {cid: int, attribute: string, ty: AST.types, notnull: bool, dflt_val: string,
                  primary_key: AST.pk option, foreign_key: AST.fk option, tables: string list}, tab) =
        {cid = #cid e, attribute = #attribute e, ty = #ty e, notnull = #notnull e, dflt_val = #dflt_val e,
        primary_key = #primary_key e, foreign_key = #foreign_key e, tables = tab}

  (* after all of the joining between tables and everything, this function changes the
     column ids and pks to be accurate in the result *)
  fun resetcid ([], _) = []
    | resetcid ((x :: xs) : {cid: int, attribute: string, ty: AST.types, notnull: bool, dflt_val: string,
                            primary_key: AST.pk option, foreign_key: AST.fk option, tables: string list} list, n) =
        (case #primary_key x
          of SOME (AST.PK _) => setFK (setPK (setNull (setCID (x, n), true), NONE), NONE) :: resetcid (xs, n + 1)
           | _ => setFK (setPK (setCID (x, n), NONE), NONE) :: resetcid (xs, n + 1))

  fun union ([], []) = []
    | union (x1 :: l1 : {cid: int, attribute: string, ty: AST.types, notnull: bool, dflt_val: string,
                        primary_key: AST.pk option, foreign_key: AST.fk option, tables: string list} list,
             x2 :: l2 : {cid: int, attribute: string, ty: AST.types, notnull: bool, dflt_val: string,
                        primary_key: AST.pk option, foreign_key: AST.fk option, tables: string list} list) =
        if (#attribute x1) = (#attribute x2) andalso (#ty x1) = (#ty x2)
        then setTables (setFK (setPK (setDflt (setNull (x1, false), ""), NONE), NONE), (#tables x1)@(#tables x2)) :: union (l1, l2)
        else raise Fail "invalid union"
    | union (_, _) = raise Fail "union must be same number of attrs"

  (* TODO: Implement this *)
  fun staysUnique (_, _, _) = []

  (* TODO: in the case of left natural join:
     2. set unique field to true if PK, leave it if not PK
     3. merge lists and remove duplicates.
        a) if both copies of duplicate are unique, then it stays unique.
        b) if you say where a = b and both a and b unique, both stay unique
        c) everyting else goes to not unique *)
  (* evaluate each term *)
  fun eval (AST.Relation rel) = AST.Relation rel
    | eval (AST.CartProd (rel1, rel2, filt)) =
        (case (eval rel1, eval rel2)
          of (AST.Relation (l1, u1), AST.Relation (l2, u2)) => AST.Relation (resetcid (l1@l2, 0), staysUnique (u1, u2, filt))
           | (_, _) => raise Fail "eval error - natural join")
    | eval (AST.Proj (("*", "*", _)::[], rel)) = eval rel
    | eval (AST.Proj (("*", "*", _)::strs, rel)) = eval (AST.CartProd (eval rel, AST.Proj (strs, rel), [])) (* TODO: this could cause the schema and relation to not match *)
    | eval (AST.Proj ((str1, str2, _)::[], rel)) =
        (case eval rel
          of AST.Relation (ls, _) =>
               if List.exists (fn x => x = #".") (explode str1)
               then (let
                      val tableAndAttr = Scan.separateRow (str1, [#"."])
                      val table = List.nth (tableAndAttr, 0)
                      val attr = List.nth (tableAndAttr, 1)
                    in
                      AST.Relation (List.map (fn x => setAttr (x, str2))
                                             (resetcid ((List.filter (fn e : {cid: int, attribute: string, ty: AST.types, notnull: bool, dflt_val: string,
                                                                             primary_key: AST.pk option, foreign_key: AST.fk option, tables: string list} =>
                                                                       attr = (#attribute e) andalso
                                                                       List.exists (fn x => x = table) (#tables e)) ls), 0)), [])
                    end)
               else AST.Relation (List.map (fn x => setAttr (x, str2))
                                           (resetcid ((List.filter (fn x => (#attribute x) = str1) ls), 0)), [])
           | _ => raise Fail "eval error - projection")
    | eval (AST.Proj (str::strs, rel)) = eval (AST.CartProd (AST.Proj (str::[], rel), AST.Proj (strs, rel), [])) (* TODO: this could cause the shema and relation to not match *)
    | eval (AST.Union (rel1, rel2)) =
        (case (eval rel1, eval rel2)
          of (AST.Relation (l1, _), AST.Relation (l2, _)) => AST.Relation (union (l1, l2), [])
           | (_, _) => raise Fail "eval error - union")
    | eval _ = raise Fail "improper input to eval"

end
