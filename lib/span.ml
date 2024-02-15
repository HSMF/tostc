type pos = int * int
type span = string * pos * pos

let none : span = "__internal", (0, 0), (0, 0)
