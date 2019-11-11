structure Eval : sig

  val eval : AST.term -> AST.term

end = struct

  (* if schema, after a natural join, has two of the same attribute, this
     function will remove one of them *)
  fun removeDuplicates ([], l) = l
    | removeDuplicates ((a, (attr, b), c, d, e, f, (g, ts))::xs, l) =
        (case List.filter (fn (_, (_, b'), _, _, _, _, _) => b = b') l
          of (_, _, _, _, _, _, (_, ts')) :: [] => (a, (attr, b), c, d, e, f, (g, ts@ts')) ::
               removeDuplicates (xs, List.filter (fn (_, (_, b'), _, _, _, _, _) => b <> b') l)
           | [] => (a, (attr, b), c, d, e, f, (g, ts)) :: removeDuplicates (xs, l)
           | _ => raise Fail "currently don't support when a relation has multiple columns of same name in natural join")

  (* after all of the joining between tables and everything, this function changes the
     column ids and pks to be accurate in the result *)
  fun resetcid ([], _) = []
    | resetcid ((((a, _), b, c, (d, _), e, (f, SOME AST.PK), g) :: xs), n) =
        ((a, n), b, c, (d, true), e, (f, NONE), g) :: resetcid (xs, n + 1)
    | resetcid ((((a, _), b, c, (d, _), e, (f, SOME (AST.FK _)), g) :: xs), n) =
        ((a, n), b, c, (d, true), e, (f, NONE), g) :: resetcid (xs, n + 1)
    | resetcid ((((a, _), b, c, d, e, (f, _), g) :: xs), n) =
        ((a, n), b, c, d, e, (f, NONE), g) :: resetcid (xs, n + 1)

  fun union ([], []) = []
    | union ((a1, (b1, attr1), (c1, type1), (d1, _), (e1, _), (f1, _), (g1, ts1)) :: l1,
              (_, (_, attr2), (_, type2), _, _, _, (g2, ts2)) :: l2) =
        if attr1 = attr2 andalso type1 = type2
        then (a1, (b1, attr1), (c1, type1), (d1, false), (e1, ""), (f1, NONE), (g1, ts1@ts2)) :: union (l1, l2)
        else raise Fail "invalid union"
    | union (_, _) = raise Fail "union must be same number of attrs"

  (* evaluate each term *)
  fun eval (AST.Relation ls) = AST.Relation ls
    | eval (AST.CartProd (rel1, rel2)) =
        (case (eval rel1, eval rel2)
          of (AST.Relation l1, AST.Relation l2) => AST.Relation (resetcid ((l1@l2), 0))
           | (_, _) => raise Fail "eval error - cartesian product")
    | eval (AST.NatJoin (rel1, rel2)) =
        (case (eval rel1, eval rel2)
          of (AST.Relation l1, AST.Relation l2) => AST.Relation (resetcid ((removeDuplicates (l1, l2)), 0))
           | (_, _) => raise Fail "eval error - natural join")
    | eval (AST.Proj (("*", "*")::[], rel)) = eval rel
    | eval (AST.Proj (("*", "*")::strs, rel)) = eval (AST.CartProd (eval rel, AST.Proj (strs, rel)))
    | eval (AST.Proj ((str1, str2)::[], rel)) =
        (case eval rel
          of AST.Relation ls =>
               if List.exists (fn x => x = #".") (explode str1)
               then (let
                      val tableAndAttr = Scan.separateRow (str1, [#"."])
                      val table = List.nth (tableAndAttr, 0)
                      val attr = List.nth (tableAndAttr, 1)
                    in
                      AST.Relation (List.map (fn (a, (b, _), c, d, e, f, g) => (a, (b, str2), c, d, e, f, g))
                                             (resetcid ((List.filter (fn (_, (_, attr'), _, _, _, _, (_, ts)) =>
                                                                       attr = attr' andalso List.exists (fn x => x = table) ts) ls), 0)))
                    end)
               else AST.Relation (List.map (fn (a, (b, _), c, d, e, f, g) => (a, (b, str2), c, d, e, f, g))
                                           (resetcid ((List.filter (fn (_, (_, attr), _, _, _, _, _) => attr = str1) ls), 0)))
           | _ => raise Fail "eval error - projection")
    | eval (AST.Proj (str::strs, rel)) = eval (AST.CartProd (AST.Proj (str::[], rel), AST.Proj (strs, rel)))
    | eval (AST.Union (rel1, rel2)) =
        (case (eval rel1, eval rel2)
          of (AST.Relation l1, AST.Relation l2) => AST.Relation (union (l1, l2))
           | (_, _) => raise Fail "eval error - union")
    | eval _ = raise Fail "improper input to eval"

end
