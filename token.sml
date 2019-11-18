structure Token = struct

  datatype token
    = SelectAll
    | SelectDistinct
    | As
    | From
    | InnerJoin
    | LeftOuterJoin
    | NatJoin
    | LeftNatJoin
    | String of string
    | Union

end
