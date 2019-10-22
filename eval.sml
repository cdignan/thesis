structure Eval : sig

  val eval : AST.term -> AST.term

end = struct

  fun removeDuplicates [] = []
    | removeDuplicates (x::xs) = x::removeDuplicates(List.filter (fn y => y <> x) xs)

  fun eval (AST.Relation ls) = AST.Relation ls
    | eval (AST.CartProd (rel1, rel2)) =
        (case (eval rel1, eval rel2)
          of (AST.Relation l1, AST.Relation l2) => AST.Relation (l1@l2)
           | (_, _) => raise Fail "eval error - cartesian product")
    | eval (AST.NatJoin (rel1, rel2)) =
        (case (eval rel1, eval rel2)
          of (AST.Relation l1, AST.Relation l2) => AST.Relation (removeDuplicates (l1@l2))
           | (_, _) => raise Fail "eval error - natural join")
    | eval (AST.Proj ("*"::[], rel)) = eval rel
    | eval (AST.Proj ("*"::strs, rel)) = eval (AST.CartProd (eval rel, AST.Proj (strs, rel)))
    | eval (AST.Proj (str::[], rel)) =
        (case eval rel
          of AST.Relation ls => AST.Relation (List.filter (fn (attr, ty) => attr = str) ls)
           | _ => raise Fail "eval error - projection")
    | eval (AST.Proj (str::strs, rel)) = eval (AST.CartProd (AST.Proj (str::[], rel), AST.Proj (strs, rel)))
    | eval _ = raise Fail "improper input to eval"

end
