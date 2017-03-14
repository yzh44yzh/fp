open Core.Std

type 'a t

val create : 'a list -> 'a t option

val left : 'a t -> 'a t option

val go_left: int -> 'a t -> 'a t option

val right : 'a t -> 'a t option

val go_right: int -> 'a t -> 'a t option

val get : 'a t -> 'a

val set : 'a -> 'a t -> 'a t

val get_position : 'a t -> int

val set_position : int -> 'a t -> 'a t option

val to_list : 'a t -> 'a list

val to_string : f:('a -> string) -> 'a t -> string
