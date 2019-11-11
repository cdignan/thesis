structure Token = struct

  datatype token
    = Select
    | As
    | From
    | NatJoin
    | String of string
    | Union

end
