open Printf

module Bdb = Camltc.Bdb

let nuke db fn =
  Camltc.Hotc.bdb_close db;
  Camltc.Hotc.bdb_delete db;
  Sys.remove fn

let () =
  let fn = "/tmp/test.tc" in
  let db = Camltc.Hotc.bdb_create fn [] in
  Bdb.put db "key" "value";
  Bdb.put db "key0" "value0";
  Bdb.put db "key1" "value1";
  Bdb.put db "key2" "value2";
  assert(Bdb.get db "key" = "value");
  let cur = Camltc.Hotc.bdb_get_cursor db in
  Bdb.last db cur;
  assert(Bdb.key db cur = "key2");
  assert(Bdb.value db cur = "value2");
  let n = Int64.to_int (Bdb.get_key_count db) in
  Bdb.first db cur;
  for i = 1 to n do
    let k = Bdb.key db cur in
    let v = Bdb.value db cur in
    printf "%s:%s\n" k v;
    if i < n then
      Bdb.next db cur
  done;
  printf "OK\n";
  nuke db fn
