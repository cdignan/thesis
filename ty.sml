structure Ty = struct

  datatype ty
    = Int of int
    | Text of string
    | Date of string
    | Time of string
    | Option of ty

end
