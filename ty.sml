structure Ty = struct

  datatype groundTy
    = Int of int
    | Text of string
    | Date of string
    | Time of string

  datatype ty
    = Type of groundTy
    | Option of groundTy option

end
