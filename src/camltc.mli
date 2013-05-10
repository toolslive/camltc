module Bdb : sig
  type bdb
  type bdbcur
  val default_mode : int
  val readonly_mode : int
  type opt = BDBTLARGE
  val put : bdb -> string -> string -> unit
  val get: bdb -> string -> string
  val out: bdb -> string -> unit 
  val range: bdb -> string option -> bool -> string option -> bool -> int -> string array
  val exists : bdb -> string -> bool
  val delete_prefix : bdb -> string -> int
  val prefix_keys: bdb -> string -> int -> string array

  val rev_range_entries: 
    string ->
    bdb ->
    string option ->
    bool -> string option -> bool -> int -> (string * string) list
  val get_key_count : bdb -> int64

  val first: bdb -> bdbcur -> unit
  val next: bdb -> bdbcur -> unit
  val prev: bdb -> bdbcur -> unit
  val last: bdb -> bdbcur -> unit
    
  val key: bdb -> bdbcur -> string
  val value: bdb -> bdbcur -> string
end

module Hotc : sig 
  type t
  type bdb = Bdb.bdb
  type bdbcur = Bdb.bdbcur
  val filename : t -> string
  val create : ?mode:int -> string -> Bdb.opt list -> t Lwt.t 
  val get_bdb: t -> bdb
  val transaction :  t ->  (bdb -> 'd Lwt.t) -> 'd Lwt.t
  val with_cursor : bdb -> (bdb -> bdbcur -> 'a Lwt.t) -> 'a Lwt.t
  val read : t -> (bdb -> 'b Lwt.t) -> 'b Lwt.t
  val optimize : t -> unit Lwt.t
  val defrag : t -> int Lwt.t
  val sync :t -> unit Lwt.t
  val close : t -> unit Lwt.t
  val reopen: t -> (unit -> unit Lwt.t) -> int -> unit Lwt.t
end


