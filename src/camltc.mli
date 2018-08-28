(*
Copyright (2010-2014) INCUBAID BVBA

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*)

val git_revision: string
val compile_time: string
val machine:string
val major:int
val minor:int
val patch:int
val dependencies: string

module Bdb : sig
  type bdb
  type bdbcur
  val default_mode : int
  val readonly_mode : int
  type opt = BDBTLARGE

  (** [put db key value] add to [db] the [key] to [value] binding. *)
  val put : bdb -> string -> string -> unit

  (** [get db key] retrieve from [db] the value associated with [key].
      @raise DOCUMENT_ME in case [key] was not found in [db]. *)
  val get: bdb -> string -> string

  (** [get3 db key] DOCUMENT_ME *)
  val get3: bdb -> string -> string

  val oreader: int
  val owriter: int
  val ocreat: int
  val otrunc: int
  val onolck: int
  val olcknb: int
  val otsync: int

  (** create a bdb for standalone use *)
  val create: ?mode:int -> ?lcnum:int -> ?ncnum:int ->
    string -> opt list -> bdb

  (** close given bdb *)
  val close: bdb -> unit

  (** delete given bdb (must have been closed before) *)
  val delete: bdb -> unit

  (** sync the given bdb *)
  val sync: bdb -> unit

  (** create a cursor for the given bdb *)
  val get_cursor: bdb -> bdbcur

  val get3_generic : bdb -> string -> int -> int -> string
  (** [get3_generic bdb s off len] considers the substring of s
      as the key for a get3
  *)

  (** [get_nolock db key] DOCUMENT_ME probably faster version
      of get on a readonly-opened database *)
  val get_nolock : bdb -> string -> string

  (** [out db key] remove from [db] the binding for [key].
      @raise DOCUMENT_ME in case [key] was not found in [db]. *)
  val out: bdb -> string -> unit

  val range: bdb ->
    string option -> bool ->
    string option -> bool -> int ->
    string array

  (** [exists db key] check if [key] is bound in [db]. *)
  val exists : bdb -> string -> bool

  val delete_prefix : bdb -> string -> int
  val prefix_keys : bdb -> string -> int -> string array

  type include_key = bool
  type upper_border =
  | BKey of string * include_key
  | BOmega

  val range_ascending : bdb -> string -> bool -> upper_border ->
    ((string * string) -> 'a -> ('a * bool)) -> 'a -> 'a
  val range_descending : bdb -> upper_border -> string -> bool ->
    ((string * string) -> 'a -> ('a * bool)) -> 'a -> 'a

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

  (** [first db cur] place cursor [cur] in front of the first
      record in [db]. *)
  val first: bdb -> bdbcur -> unit

  (** [jump db cur key] place cursor [cur] in front of the
      record with key [key] in [db]. *)
  val jump : bdb -> bdbcur -> string -> unit

  (** [next db cur] place cursor [cur] in front of the next
      record in [db] *)
  val next : bdb -> bdbcur -> unit

  (** [prev db cur] place cursor [cur] in front of the previous
      record in [db].
      DOCUMENT_ME what does it do when we were already on the first? *)
  val prev : bdb -> bdbcur -> unit

  (** [last db cur] place cursor [cur] in front of the last
      record in [db] *)
  val last : bdb -> bdbcur -> unit

  (** [cur_out db cur] delete the key value pair pointed to by [cur]
      in [db] and go to the next one *)
  val cur_out  : bdb -> bdbcur -> unit

  (** [key db cur] get the key currently pointed to by cursor [cur] *)
  val key: bdb -> bdbcur -> string
    
  (** [value db cur] get the value currently pointed to by cursor [cur] *)
  val value: bdb -> bdbcur -> string

  (** [key3 db cur] DOCUMENT_ME *)
  val key3: bdb -> bdbcur -> string

  (** [value3 db cur] DOCUMENT_ME *)
  val value3: bdb -> bdbcur -> string

  (** [with_cursor db f] DOCUMENT_ME *)
  val with_cursor : bdb -> (bdb -> bdbcur -> 'a) -> 'a

  val _tranbegin: bdb -> unit
  val _trancommit: bdb -> unit
  val _tranabort: bdb -> unit

  type flag = BDBFOPEN | BDBFFATAL
  val flags: bdb -> flag list

  (** [defrag ~step db] DOCUMENT_ME *)
  val defrag : ?step:int64 -> bdb -> int

  val copy_from_cursor : source:bdb -> cursor:bdbcur -> target:bdb -> max:int option -> int
end

module Hotc : sig
  type t
  type bdb = Bdb.bdb
  type bdbcur = Bdb.bdbcur
  val filename : t -> string
  val create : ?mode:int ->
    ?lcnum:int ->
    ?ncnum:int ->
    string -> Bdb.opt list -> t Lwt.t
  val get_bdb: t -> bdb
  val transaction :  t ->  (bdb -> 'd Lwt.t) -> 'd Lwt.t
  val with_cursor : bdb -> (bdb -> bdbcur -> 'a Lwt.t) -> 'a Lwt.t
  val read : t -> (bdb -> 'b Lwt.t) -> 'b Lwt.t
  val optimize : t -> unit Lwt.t
  val defrag : ?step:int64 -> t -> int Lwt.t
  val do_locked : t -> (unit -> 'a Lwt.t) -> 'a Lwt.t
  val sync : t -> unit
  val sync_nolock : t -> unit
  val close : t -> unit Lwt.t
  val reopen: t -> (unit -> unit Lwt.t) -> int -> unit Lwt.t
end
