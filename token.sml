structure Token = struct

  datatype token
    = SelectAll
    | SelectDistinct
    | As
    | From
    | NatJoin
    | String of string
    | Union

end
