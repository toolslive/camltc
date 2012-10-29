open OUnit

let suite = "tokyo" >::: [Otc_test.suite;Hotc_test.suite]

let _ = run_test_tt_main suite 

(* flag ["ocaml";"link";"is_main"](
      S[A"-thread";
        A"-linkpkg";
        (*A"-ccopt"; A("-L" ^ tc_home);*)
        (*A"-ccopt"; A"-ltokyocabinet";*)
        A"src/libcutil.a";
        A"src/otc/libotc.a";
        A (tc_home ^ "/libtokyocabinet.a");
*);;
