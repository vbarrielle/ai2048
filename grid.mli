
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

(** The grid after being moved. The move can have had no effect,
 *  have succeeded, or have caused a game over
 *)
type move_res =
  | Good of t
  | Useless of t
  | Game_over of t

(** Perform a move on a grid *)
val move : t -> move -> move_res

(** Return the grid as a list of lists *)
val to_llist : t -> int option list list

(** Pretty print the grid *)
val to_string : t -> string

(** Pretty print a move *)
val move_to_string: move -> string

(** Get a list of empty locations on a grid. Each location
 * is the index of the empty case in the row-major ordering of the
 * grid. *)
val empty_locations: t -> int list

(** Evaluate the game score.
 *
 * The game score is history based, it is the sum of the values of
 * all tiles created by a merge.
 *)
val eval_pos : t -> float

(** Evaluate the game position using a heuristic.
 *
 * The value of the position is given by the sum of the values of
 * the grid divided by the number of non-empty cases
 *)
val heuristic_eval_pos : t -> float

(** Value of the highest grid entry *)
val highest : t -> int

(** Number of empty cases in a grid *)
val nb_empty: t -> int
