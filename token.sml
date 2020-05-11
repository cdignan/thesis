structure Token = struct

  datatype token
    = SelectAll
    | SelectDistinct
    | As
    | From
    | CartProd
    | String of string
    | Where
    | Equals
    | And
    | Union

end
