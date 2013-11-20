val git_revision: string
val compile_time: string
val machine:string
val major:int
val minor:int
val patch:int
val dependencies: string


module Bdb : sig
  module OpenMode : sig
    type t = OREADER
           | OWRITER
           | OCREAT
           | OTRUNC
           | ONOLCK
           | OLCKNB
           | OTSYNC
  end

  type bdb
  type bdbcur
  val default_mode : OpenMode.t list
  val readonly_mode : OpenMode.t list
  type opt = BDBTLARGE
  val put : bdb -> string -> string -> unit
  val get: bdb -> string -> string
  val out: bdb -> string -> unit
  val range: bdb ->
    string option -> bool ->
    string option -> bool -> int ->
    string array


  val exists : bdb -> string -> bool
  val delete_prefix : bdb -> string -> int
  val prefix_keys: bdb -> string -> int -> string array


  val range_entries : string ->
    bdb ->
    string option -> bool ->
    string option -> bool -> int ->
    (string * string) array

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

  val _tranbegin: bdb -> unit
  val _trancommit: bdb -> unit
  val _tranabort: bdb -> unit

  type flag = BDBFOPEN | BDBFFATAL
  val flags: bdb -> flag list

  val defrag : ?step:int64 -> bdb -> int
end

module Hotc : sig
  type t
  type bdb = Bdb.bdb
  type bdbcur = Bdb.bdbcur
  val filename : t -> string
  val create : ?mode:Bdb.OpenMode.t list ->
    ?lcnum:int ->
    ?ncnum:int ->
    string -> Bdb.opt list -> t Lwt.t
  val get_bdb: t -> bdb
  val transaction :  t ->  (bdb -> 'd Lwt.t) -> 'd Lwt.t
  val with_cursor : bdb -> (bdb -> bdbcur -> 'a Lwt.t) -> 'a Lwt.t
  val read : t -> (bdb -> 'b Lwt.t) -> 'b Lwt.t
  val optimize : t -> unit Lwt.t
  val defrag : ?step:int64 -> t -> int Lwt.t
  val sync :t -> unit Lwt.t
  val close : t -> unit Lwt.t
  val reopen: t -> (unit -> unit Lwt.t) -> Bdb.OpenMode.t list -> unit Lwt.t
end
