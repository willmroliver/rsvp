type string_proof = unit
type +'proof t

val of_string : string -> unit t
val to_string : 'proof t -> string

type 'p non_empty
type 'p alpha
type 'p numeric
type 'p alpha_numeric

val non_empty : 'p t -> 'p non_empty t option
val alpha : 'p t -> 'p alpha t option
val numeric : 'p t -> 'p numeric t option
val alpha_numeric : 'p t -> 'p alpha_numeric t option
