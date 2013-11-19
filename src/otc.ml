(* use Hotc for highlevel locked access *)

let next_prefix prefix =
  let next_char c =
    let code = Char.code c + 1 in
    match code with
      | 256 -> Char.chr 0, true
      | code -> Char.chr code, false in
  let rec inner s pos =
    let c, carry = next_char s.[pos] in
    s.[pos] <- c;
    match carry, pos with
      | false, _ -> Some s
      | true, 0 -> None
      | true, pos -> inner s (pos - 1) in
  let copy = String.copy prefix in
  inner copy ((String.length copy) - 1)

let prefix_match prefix k =
  let pl = String.length prefix in
  let rec ok i = (i = pl) || (prefix.[i] = k.[i] && ok (i+1)) in
  String.length k >= pl && ok 0

module Bdb = struct

  type bdb (* type stays abstract *)

  let oreader = 1
  let owriter = 2
  let ocreat  = 4
  let otrunc  = 8
  let onolck  = 16
  let olcknb  = 32
  let otsync  = 64

  let default_mode = (oreader lor owriter lor ocreat lor olcknb)
  let readonly_mode = (oreader lor onolck)

  type bdbcur (* type stays abstract *)

  external first: bdb -> bdbcur -> unit = "bdb_first"
  external next: bdb -> bdbcur -> unit = "bdb_next"
  external prev: bdb -> bdbcur -> unit = "bdb_prev"
  external last: bdb -> bdbcur -> unit = "bdb_last"
  external key: bdb -> bdbcur -> string = "bdb_key"
  external value: bdb -> bdbcur -> string = "bdb_value"
  external record: bdb -> bdbcur -> string * string = "bdb_record"
  external jump: bdb -> bdbcur -> string -> unit = "bdb_jump"

  let current = 0
  let before = 1
  let after = 2

  external cur_put: bdb -> bdbcur -> string -> int -> unit = "bdb_cur_put"
  external cur_out: bdb -> bdbcur -> unit = "bdb_cur_out"

  external out: bdb -> string -> unit = "bdb_out"
  external put: bdb -> string -> string -> unit = "bdb_put"
  external get: bdb -> string -> string = "bdb_get"
  external putkeep: bdb -> string -> string -> unit = "bdb_putkeep"

  (* TODO: let getters return a "string option" isof throwing Not_found *)

  (* TODO: maybe loose the delete calls and hook it up in GC *)

    (* don't use these directly , use Hotc *)
  external _make: unit -> bdb   = "bdb_make"
  external _delete: bdb -> unit = "bdb_delete"

  external _dbopen: bdb -> string -> int -> unit = "bdb_dbopen"
  external _dbclose: bdb -> unit                 = "bdb_dbclose"
  external _dbsync: bdb -> unit                  = "bdb_dbsync"

  external _cur_make: bdb -> bdbcur = "bdb_cur_make"
  external _cur_delete: bdbcur -> unit = "bdb_cur_delete"

  external _tranbegin: bdb -> unit = "bdb_tranbegin"
  external _trancommit: bdb -> unit = "bdb_trancommit"
  external _tranabort: bdb -> unit = "bdb_tranabort"

  external range: bdb -> string option -> bool -> string option -> bool -> int -> string array
    = "bdb_range_bytecode" "bdb_range_native"

  external prefix_keys: bdb -> string -> int -> string array = "bdb_prefix_keys"
  external bdb_optimize: bdb -> unit = "bdb_optimize"

  external _bdb_defrag: bdb -> int64 -> int = "bdb_defrag"
  let defrag ?(step=Int64.max_int) bdb = _bdb_defrag bdb step

  external get_key_count: bdb -> int64 = "bdb_key_count"

  external setcache: bdb -> int -> int -> unit = "bdb_setcache"

  type opt = BDBTLARGE
  external _tune : bdb -> (* int -> int -> int -> int -> int -> *) int -> unit = "bdb_tune"
  let tune bdb opts =
    let int_of_opt = function
      BDBTLARGE -> 1 lsl 0
    in
    let int_of_opts = List.fold_left (fun a b -> a lor int_of_opt b) 0 in
    _tune bdb (int_of_opts opts)

  let with_cursor2 bdb (f:bdb -> 'a) =
    let cursor = _cur_make bdb in
    try
      let x = f bdb cursor in
      let () = _cur_delete cursor in
      x
    with
      | exn ->
        let () = _cur_delete cursor in
        raise exn


  let delete_prefix bdb prefix =
    let count = ref 0 in
    with_cursor2 bdb
      (fun bdb cur ->
        try
          let () = jump bdb cur prefix in
          let rec step () =
            let jumped_key = key bdb cur in
            if prefix_match prefix jumped_key
            then
              let () = cur_out bdb cur in (* and jump to next *)
              let () = incr count in
              step ()
            else
              ()

          in
          step ()
        with
          | Not_found -> ()
      );
    !count


  let exists bdb key =
    try
      let _ = get bdb key in true
    with
      | Not_found -> false

  type direction =
  | Ascending
  | Descending

  type include_key = bool

  type start_and_direction =
  | Key of string * include_key * direction
  | OmegaDescending

  let range'
      bdb
      start_key_and_direction
      (accumulate : (string * string) -> 'a -> ('a * bool))
      (initial : 'a) : 'a =
    let cursor_init, move_next =
      match start_key_and_direction with
      | Key (start_key, include_key, dir) ->
        begin
          match dir with
          | Ascending ->
            let skip_till_start_key bdb cur =
              try
                jump bdb cur start_key;
                if include_key
                then
                  ()
                else
                  next bdb cur
              with Not_found ->
                ()
            in
            skip_till_start_key, next
          | Descending ->
            let init_cur bdb cur =
              try
                jump bdb cur start_key;
                if include_key
                then
                  ()
                else
                  prev bdb cur
              with Not_found ->
                last bdb cur
            in
            init_cur, prev
        end
      | OmegaDescending ->
        last, prev in
    with_cursor2 bdb
      (fun bdb cur ->
        let () = cursor_init bdb cur in
        let rec loop (acc, continue) =
          if not continue
          then
            acc
          else
            begin
              let record_ = record bdb cur in
              let (acc', _) as res = accumulate record_ acc in
              try
                let () = move_next bdb cur in
                loop res
              with Not_found ->
                acc'
            end in
        loop (initial, true))

  let range_entries prefix bdb first finc last_ linc max =
    let pl = String.length prefix in
    let first  = match first with
      | Some x -> prefix ^ x
      | None   -> prefix in
    let comp key =
      match last_ with
      | None ->
        -1
      | Some x ->
        String.compare key (prefix ^ x) in
    let _, result =
      range'
        bdb
        (Key (first, finc, Ascending))
        (fun (key, value) (count, result) ->
          let return include_kv continue =
            if include_kv
            then
              let l = String.length key in
              let key2 = String.sub key pl (l - pl) in
              let result' = (key2, value) :: result in
              ((count + 1, result'), continue)
            else
              ((count, result), continue) in
          if count = max
          then
            return false false
          else
            begin
              let comp' = comp key in
              if comp' = 0
              then
                return linc false
              else if comp' = 1
              then
                return false false
              else (* comp' = -1 *)
                return true true
            end)
        (0, []) in
    Array.of_list (List.rev result)

  let rev_range_entries prefix bdb first finc last_ linc max =
    let pl = String.length prefix in
    let last_ = match last_ with
      | None -> ""
      | Some x -> prefix ^ x in
    let start_and_direction =
      match first with
      | None ->
        begin
          match next_prefix prefix with
          | None ->
            OmegaDescending
          | Some x ->
            Key (x, false, Descending)
        end
      | Some x ->
        Key (prefix ^ x, finc, Descending) in
    let _, result =
      range'
        bdb
        start_and_direction
        (fun (key, value) (count, result) ->
          let l = String.length key in
          let return include_kv continue =
            if include_kv
            then
              let key2 = String.sub key pl (l - pl) in
              let result' = (key2, value) :: result in
              ((count + 1, result'), continue)
            else
              ((count, result), continue) in
          if count = max
          then
            return false false
          else
            begin
              if not (prefix_match prefix key) then
                return false false
              else
                if last_ = key
                then
                  begin
                    if linc
                    then
                      return true false
                    else
                      return false false
                  end
                else if last_ > key
                then
                  return false false
                else
                  return true true
            end)
        (0, []) in
    result


  external _flags : bdb -> int = "bdb_flags"
  type flag = BDBFOPEN | BDBFFATAL

  let flags bdb =
    let f = _flags bdb in
    List.fold_left
      (fun acc (s, c) -> if f land c <> 0 then s :: acc else acc)
      []
      (* Shifts taken from tcbdb.h and tchdb.h *)
      [(BDBFOPEN, 1 lsl 0); (BDBFFATAL, 1 lsl 1)]
end
