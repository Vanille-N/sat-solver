(** Support for hexagonal grids as used in Hey That's My Fish! *)

(** {2 Grids and positions} *)

(** A hexagonal grid is represented as a matrix. The details on the
  * mapping from hexagonal to square grids do not matter. Positions
  * in the grid are thus simply pairs of integers.
  *
  * We make use of [char grid] when printing, and [bool grid] when
  * we only need to know which cells are avaiable. *)

type pos = int * int
type 'a grid = 'a array array

(** {2 Movement} *)

(** There are six directions for moving: north, north-east, etc. *)
type dir = N | NE | SE | S | SW | NW

(** List of all possible directions. *)
val all_directions : dir list

(** A movement is a direction and an integer indicating how many
  * steps must be taken in that direction. *)
type move = dir * int

(** [move p d] indicates the position obtained when moving from [p]
  * in direction [d] with just one step. *)
val move : pos -> dir -> pos

(** Compute the position resulting from an arbitrary move. *)
val move_n : pos -> move -> pos

(** {2 Parsing and printing} *)

(** Display grids on formatters. *)

val pp_char_grid : Format.formatter -> char grid -> unit
val pp_bool_grid : Format.formatter -> bool grid -> unit

(** [pp_solution fmt grid path] displays the grid on [fmt],
  * and shows a path (list of positions) on it. It is meant to
  * be used to visualize problem solutions. *)
val pp_solution :
  Format.formatter -> bool grid -> pos array -> unit

(** Read a grid and initial position from some input channel. *)
val from_channel : in_channel -> pos * bool grid
