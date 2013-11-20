module Hotc : sig
  type t
  type bdb = Otc.Bdb.bdb
  type bdbcur = Otc.Bdb.bdbcur

  val filename : t -> string
  val transaction :  t -> (bdb -> 'd Lwt.t) -> 'd Lwt.t
  val with_cursor :  bdb -> (bdb -> Otc.Bdb.bdbcur -> 'e Lwt.t) -> 'e Lwt.t
  val batch : t -> int -> string -> string option -> (string * string) list Lwt.t
  val get_bdb : t -> bdb
  val read : t -> (bdb -> 'b Lwt.t) -> 'b Lwt.t
  val create: ?mode:Otc.Bdb.OpenMode.t list -> ?lcnum:int -> ?ncnum:int ->
    string -> Otc.Bdb.opt list -> t Lwt.t
  val delete: t -> unit Lwt.t
  val optimize: t -> unit Lwt.t
  val reopen: t -> (unit -> unit Lwt.t) -> Otc.Bdb.OpenMode.t list -> unit Lwt.t
  val sync : t -> unit Lwt.t
  val close : t -> unit Lwt.t
  val defrag : ?step:int64 -> t -> int Lwt.t
end
