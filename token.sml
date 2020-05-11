structure Token = struct

  datatype token
    = SelectAll
    | SelectDistinct
    | As
    | From
    | NatJoin
    | LeftNatJoin
    | String of string
    | Union

end
