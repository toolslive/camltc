
(* use Hotc for highlevel locked access *)

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

  external bdb_defrag: bdb -> int = "bdb_defrag"
  external get_key_count: bdb -> int64 = "bdb_key_count"

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
      let _ = get bdb key in
	  true
    with 
      | Not_found -> false

  (* some remarks: normally you'll want yo use this function with a
     first parameter. Without a first parameter the FIRST entry in
     the prefix will be first, this is probably not very useful
     when reverse paging *)
  let rev_range_entries prefix bdb first finc last_ linc max =
    let first = match first with | None -> prefix | Some x -> prefix ^ x in
    let last_ = match last_ with | None -> "" | Some x -> prefix ^ x in
    let pl = String.length prefix in
    try with_cursor2 bdb (fun bdb cur ->
      let () =
        try
          jump bdb cur first
        with
          | Not_found -> last bdb cur
      in
      let jumped_key = key bdb cur in
      let jumped_key_prefix = String.sub jumped_key 0 pl in
      let () = if not finc || jumped_key_prefix <> prefix then prev bdb cur in
      let rec rev_e_loop acc count =
        if count = max then acc
        else
          let key, value = record bdb cur in
          let l = String.length key in
          (* make sure we have a key length long enough to contain the prefix *)
          if l < pl then acc
          else
            (* make sure the prefix is still the wanted prefix *)
            let prefix2 = String.sub key 0 pl in
            if prefix2 <> prefix then acc
            else
              let key2 = String.sub key pl (l-pl) in
              if last_ = key then
                if linc then (key2,value)::acc else acc
              else if last_ > key then acc
              else
                let acc = (key2,value)::acc in
                let maybe_next =
                  try
                    let () = prev bdb cur in
                    None
                  with
                    | Not_found ->
                      Some acc
                in
                match maybe_next with
                  | Some acc -> acc
                  | None -> rev_e_loop acc (count+1)
      in
      rev_e_loop [] 0
    )
    with
      | Not_found -> []

end
