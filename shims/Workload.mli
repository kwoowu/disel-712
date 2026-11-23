val generate : n:int -> len:int -> vmax:int -> keys:int -> seed:int -> int list list
val to_text : (int list * int) list -> string
val digest : (int list * int) list -> string
val check : int list list -> (int list * int) list -> bool
