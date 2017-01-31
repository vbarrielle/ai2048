
open Core.Std

(** The grid type *)
type t

(** Create a new game grid *)
val new_game : unit -> t

val grid_size : int

(** The moves that can be applied to a grid *)
type move =
  | Left
  | Right
  | Up
  | Down


(** Perform a move on a grid *)
val move : t -> move -> (t, t) Result.t

(** Return the grid as a list of lists *)
val to_llist : t -> int option list list

val to_string : t -> string
