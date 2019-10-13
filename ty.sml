structure Ty = struct

  datatype ty
    = Int
    | Unit
    | Text
    | Date
    | Time
    | Record of (string * ty) list

end
