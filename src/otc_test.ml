open OUnit
open Otc
open Logging
open Extra

let setup_tc _ =
  let db = Bdb._make () in
  let _ = Bdb._dbopen db "/tmp/db1.tc" (Bdb.owriter lor Bdb.ocreat lor Bdb.otrunc) in
    db

let teardown_tc db =
  let _ = Bdb._dbclose db in
  let _ = Bdb._delete db in
  let _ = Unix.unlink "/tmp/db1.tc" in
    ()

let test_make () =
  let db = Bdb._make () in
  let _ = Bdb._delete db in
    ()

let test_open db = ()

let test_basic db =
  let _ = Bdb.put db "hello" "world" in
  let v = Bdb.get db "hello" in
  let _ = Extra.eq_string "put/get" "world" v in
    ()

let test_cursor db =
  let _ = Bdb.put db "key1" "value1" in
  let _ = Bdb.put db "key2" "value2" in
  let _ = Bdb.put db "key3" "value3" in
  let cur = Bdb._cur_make db in
  let _ = Bdb.first db cur in
  let _ = eq_string "key1" "key1" (Bdb.key db cur) in
  let _ = eq_string "value1" "value1" (Bdb.value db cur) in
  let key,value = Bdb.record db cur in
  let _ = eq_string "key1" "key1" key in
  let _ = eq_string "value1" "value1" value in
  let _ = Bdb.next db cur in
  let _ = eq_string "key2" "key2" (Bdb.key db cur) in
  let _ = eq_string "value2" "value2" (Bdb.value db cur) in
  let _ = Bdb.next db cur in
  let _ = eq_string "key3" "key3" (Bdb.key db cur) in
  let _ = eq_string "value3" "value3" (Bdb.value db cur) in
  let _ = try Bdb.next db cur; assert_failure "expecting failure" with _ -> () 
  in
  let _ = Bdb._cur_delete cur in
  ()

let test_range db =
  Bdb.put db "kex1" "value1";
  Bdb.put db "key2" "value2";
  Bdb.put db "key3" "value3";
  Bdb.put db "kez3" "value4";
  let a = Bdb.range db (Some "key") true (Some "kez") false (-1) in
  let _ = eq_int "num==2" 2 (Array.length a) in
  let _ = eq_string "key2" "key2" a.(0) in
  let _ = eq_string "key3" "key3" a.(1) in
    ()

let test_unknown db =
  let _ = try Bdb.get db "hello" with
    | Not_found -> ""
    | _ -> assert_failure "unexpected exception"
  in ()

let test_prefix_keys db =
  let _ = Bdb.put db "kex1" "value1" in
  let _ = Bdb.put db "key2" "value2" in
  let _ = Bdb.put db "key3" "value3" in
  let _ = Bdb.put db "kez3" "value4" in
  let a = Bdb.prefix_keys db "key" (-1) in
  let _ = eq_int "num==2" 2 (Array.length a) in
  let _ = eq_string "key2" "key2" a.(0) in
  let _ = eq_string "key3" "key3" a.(1) in
    ()

let test_null db =
  let str = String.make 5 (char_of_int 0) in
  let () = Bdb.put db "key1" str in
  let str2 = Bdb.get db "key1" in
  let () = eq_int "length==5" 5 (String.length str2) in
  let () = eq_int "char0" 0 (int_of_char str2.[0]) in
  let () = eq_int "char0" 0 (int_of_char str2.[1]) in
  let () = eq_int "char0" 0 (int_of_char str2.[2]) in
  let () = eq_int "char0" 0 (int_of_char str2.[3]) in
  let () = eq_int "char0" 0 (int_of_char str2.[4]) in
    ()


let suite =
  let wrap f = bracket setup_tc f teardown_tc in
  "Otc" >:::
    [
      "make" >:: test_make;
      "open" >:: wrap test_open;
      "basic" >:: wrap test_basic;
      "cursor" >:: wrap test_cursor;
      "range" >:: wrap test_range;
      "unknown" >:: wrap test_unknown;
      "prefix_keys" >:: wrap test_prefix_keys;
      "null" >:: wrap test_null;
    ]
