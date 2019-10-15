structure Parse : sig

  val parse : Token.token list -> AST.queryTerm

end = struct

  fun parse tokens =
    let
      fun toList (strs, strs2, "from", []) = AST.Select (strs, strs2)
        | toList (strs, strs2, "from", Token.String str::ts) = toList (strs, strs2@[str], "from", ts)
        | toList (strs, [], "select", Token.From :: ts) = toList (strs, [], "from", ts)
        | toList (strs, [], "select", Token.String str :: ts) = toList (strs@[str], [], "select", ts)
        | toList ([], [], mode, Token.Select :: ts) = toList ([], [], "select", ts)
        | toList (_, _, _, _) = raise Fail "improper query format"
    in
      toList ([], [], "", tokens)
    end

end
