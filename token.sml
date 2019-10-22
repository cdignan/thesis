structure Token = struct

  datatype token
    = Select
    | From
    | NatJoin
    | String of string

end
