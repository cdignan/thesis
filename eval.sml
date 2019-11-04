structure Eval : sig

  val eval : AST.term -> AST.term

end = struct

  (* if schema, after a natural join, has two of the same attribute, this
     function will remove one of them *)
  fun removeDuplicates [] = []
    | removeDuplicates ((a, b, c, d, e, f)::xs) =
        (a, b, c, d, e, f)::removeDuplicates(List.filter (fn (a', b', c', d', e', f') => b <> b') xs)

  (* after all of the joining between tables and everything, this function changes the
     column ids to be accurate in the result *)
  fun resetcid ([], _) = []
    | resetcid (((a, b, c, d, e, f) :: xs), n) = (n, b, c, d, e, f) :: resetcid (xs, n + 1)

  (* evaluate each term *)
  fun eval (AST.Relation ls) = AST.Relation ls
    | eval (AST.CartProd (rel1, rel2)) =
        (case (eval rel1, eval rel2)
          of (AST.Relation l1, AST.Relation l2) => AST.Relation (resetcid ((l1@l2), 0))
           | (_, _) => raise Fail "eval error - cartesian product")
    | eval (AST.NatJoin (rel1, rel2)) =
        (case (eval rel1, eval rel2)
          of (AST.Relation l1, AST.Relation l2) => AST.Relation (resetcid ((removeDuplicates (l1@l2)), 0))
           | (_, _) => raise Fail "eval error - natural join")
    | eval (AST.Proj ("*"::[], rel)) = eval rel
    | eval (AST.Proj ("*"::strs, rel)) = eval (AST.CartProd (eval rel, AST.Proj (strs, rel)))
    | eval (AST.Proj (str::[], rel)) =
        (case eval rel
          of AST.Relation ls => AST.Relation (resetcid ((List.filter (fn (cid, attr, ty, notnull, dfltval, pk) => attr = str) ls), 0))
           | _ => raise Fail "eval error - projection")
    | eval (AST.Proj (str::strs, rel)) = eval (AST.CartProd (AST.Proj (str::[], rel), AST.Proj (strs, rel)))
    | eval _ = raise Fail "improper input to eval"

end
