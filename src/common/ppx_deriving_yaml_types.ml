module type Runtime = sig
  type t
  type integer

  val int : int -> t
  val float : float -> t
  val bool : bool -> t
  val string : string -> t
  val list : t list -> t
  val obj : (string * t) list -> t
  val null : t
  val of_integer : integer -> int
end
