
let nuke db fn =
  Camltc.Hotc.bdb_close db;
  Camltc.Hotc.bdb_delete db;
  Sys.remove fn

let () =
  let fn = "/tmp/test.tc" in
  let db = Camltc.Hotc.bdb_create fn [] in
  nuke db fn
