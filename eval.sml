structure Eval : sig

  val eval : AST.term -> AST.term

end = struct

  fun setAttr (e : AST.attr, attr) =
        {attribute = attr, ty = #ty e, notnull = #notnull e, dflt_val = #dflt_val e, tables = #tables e}

  fun setTy (e : AST.attr, typ) =
        {attribute = #attribute e, ty = typ, notnull = #notnull e, dflt_val = #dflt_val e, tables = #tables e}

  fun setNull (e : AST.attr, b) =
        {attribute = #attribute e, ty = #ty e, notnull = b, dflt_val = #dflt_val e, tables = #tables e}

  fun setDflt (e : AST.attr, dflt) =
        {attribute = #attribute e, ty = #ty e, notnull = #notnull e, dflt_val = dflt, tables = #tables e}

  fun setTables (e : AST.attr, tab) =
        {attribute = #attribute e, ty = #ty e, notnull = #notnull e, dflt_val = #dflt_val e, tables = tab}

  fun union ([], []) = []
    | union (x1 :: l1 : AST.attr list, x2 :: l2 : AST.attr list) =
        if (#attribute x1) = (#attribute x2) andalso (#ty x1) = (#ty x2)
        then setTables (setDflt (setNull (x1, false), ""), (#tables x1)@(#tables x2)) :: union (l1, l2)
        else raise Fail "invalid union"
    | union (_, _) = raise Fail "union must be same number of attrs"

  (* checks if xs is a subset of ys *)
  fun isSuperkey ([], _) = true
    | isSuperkey (x :: xs, ys) = if List.exists (fn x' => x' = x) ys then isSuperkey (xs, ys) else false

  (* xs : set of strings, u::us : set of sets of strings, return which from u::us xs is a subset of *)
  fun subsetOfTheseKeys (_, []) = []
    | subsetOfTheseKeys (xs, u :: us) = if isSuperkey (xs, u) then u :: subsetOfTheseKeys (xs, us) else subsetOfTheseKeys (xs, us)

  fun supersetOfTheseKeys (_, []) = []
    | supersetOfTheseKeys (xs, u :: us) = if isSuperkey (u, xs) then u :: supersetOfTheseKeys (xs, us) else supersetOfTheseKeys (xs, us)

  fun isAttrInRelation ([], _) = false
    | isAttrInRelation ((e :: es) : AST.attr list, attr) =
        if #attribute e = attr
        then true
        else isAttrInRelation (es, attr)

  fun split (_, _, [], res1, res2) = (res1, res2)
    | split (l1, l2, (attr1, attr2) :: attrs, res1, res2) = if isAttrInRelation (l1, attr1)
                                                            then split (l1, l2, attrs, attr1 :: res1, attr2 :: res2)
                                                            else split (l1, l2, attrs, attr2 :: res1, attr1 :: res2)

  fun unique ([], _, _, _) = []
    | unique (u1 :: u1s, u2s, filt1, filt2) = if isSuperkey (u1, filt1)
                                              then (subsetOfTheseKeys (filt2, u2s))@(unique (u1s, u2s, filt1, filt2))
                                              else unique (u1s, u2s, filt1, filt2)

  fun staysUnique (l1, l2, filt, u1, u2) = (case split (l1, l2, filt, [], [])
                                              of (res1, res2) => (unique (u1, u2, res1, res2))@(unique (u2, u1, res2, res1)))

  (* evaluate each term *)
  fun eval (AST.Relation rel) = AST.Relation rel
    | eval (AST.CartProd (rel1, rel2, filt)) =
        (case (eval rel1, eval rel2)
          of (AST.Relation (l1, u1, _, _), AST.Relation (l2, u2, _, _)) => AST.Relation ((l1@l2), staysUnique (l1, l2, filt, u1, u2), NONE, NONE)
           | (_, _) => raise Fail "eval error - natural join")
    | eval (AST.Proj (("*", "*", _)::[], rel)) = eval rel
    | eval (AST.Proj (strs, rel)) =
        (case eval rel
          of AST.Relation (l, u, _, _) =>
               let
                 val orig = List.map (fn (str1, _, _) => str1) strs
                 val u' = supersetOfTheseKeys (orig, u)
                 fun renameUniq ((_, _, _), []) = []
                   | renameUniq ((str1, str2, b), uniq :: uniqs) = (List.map (fn x => if x = str1 then str2 else x) uniq) :: renameUniq ((str1, str2, b), uniqs)
                 fun rUniq ([], _) = []
                   | rUniq ((str1, str2, b) :: strings, uniqs) = rUniq (strings, renameUniq ((str1, str2, b), uniqs))
                 fun reorder ([], []) = []
                   | reorder (s :: ss, ls : AST.attr list) =
                       (case List.partition (fn x : AST.attr => #attribute x = s) ls
                         of (a :: [], aa) => a :: reorder (ss, aa)
                          | _ => raise Fail "invalid select")
                   | reorder (_, _) = raise Fail "improper reorder format"
                 fun rename ([], []) = []
                   | rename (s :: ss, a :: aa : AST.attr list) = (setAttr (a, s)) :: rename (ss, aa)
                   | rename (_, _) = raise Fail "improper rename format"
                 val renamed = List.map (fn (_, str2, _) => str2) strs
                 val columns = List.filter (fn e : AST.attr => List.exists (fn x => if List.exists (fn y => y = #".") (explode x)
                                                                                then let
                                                                                       val tableAndAttr = Scan.separateRow (x, [#"."])
                                                                                       val table = List.nth (tableAndAttr, 0)
                                                                                       val attr = List.nth (tableAndAttr, 1)
                                                                                     in
                                                                                       attr = #attribute e andalso List.exists (fn z => z = table) (#tables e)
                                                                                     end
                                                                                else x = #attribute e) orig) l
                 val l' = rename (renamed, reorder (orig, columns))
               in
                 AST.Relation (l', rUniq (strs, u'), NONE, NONE)
               end
           | _ => raise Fail "invalid relation")
    | eval (AST.Union (rel1, rel2)) =
        (case (eval rel1, eval rel2)
          of (AST.Relation (l1, _, _, _), AST.Relation (l2, _, _, _)) => AST.Relation (union (l1, l2), [], NONE, NONE)
           | (_, _) => raise Fail "eval error - union")

end
