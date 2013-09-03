open OUnit

let suite = "tokyo" >::: [Otc_test.suite;
                          Hotc_test.suite;
                         ]

open Camltc

let dump () =
  Printf.printf ("git_revision: %S\n" ^^
    "compile_time: %S\n" ^^
    "machine: %S\n" ^^
    "major: %i\n" ^^
    "minor: %i\n" ^^
    "patch: %i\n" ^^
    "dependencies:%S\n")
    git_revision compile_time machine major minor patch dependencies

let _ =
  let () = dump () in
  run_test_tt_main suite

(* flag ["ocaml";"link";"is_main"](
      S[A"-thread";
        A"-linkpkg";
        (*A"-ccopt"; A("-L" ^ tc_home);*)
        (*A"-ccopt"; A"-ltokyocabinet";*)
        A"src/libcutil.a";
        A"src/otc/libotc.a";
        A (tc_home ^ "/libtokyocabinet.a");
*);;
