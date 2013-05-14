open Ocamlbuild_pack
open Ocamlbuild_plugin
open Unix

let run_and_read = Ocamlbuild_pack.My_unix.run_and_read
let tc_home ="3rd-party/tokyocabinet-camltc-0.6.0"

let _ = dispatch & function
  | After_rules ->
    rule "Extract .tar.gz"
      ~dep:"3rd-party/%(f).tar.gz"
      ~stamp:"3rd-party/%(f).extracted"
      begin fun env _build ->
	    let archive = env "3rd-party/%(f).tar.gz" in
	    let () = Log.dprintf 0 "extracting %s" archive in
	    let cmd =
	      Cmd (S[(*A"echo";*)
	        A"tar";
	        A"zxvf";
	        A archive;
	        A"--directory";A"3rd-party";
	      ]) in
	    Seq[cmd;]
      end;

    rule "configure 3rd-party"
      ~dep:"3rd-party/%(dir).extracted"
      ~stamp:"3rd-party/%(dir).configured"
      begin fun env _build ->
	    let dir =  env "%(dir)" in
	    let () = Log.dprintf 0 "configuring %s" dir in
	    let evil_cmd =
	      Printf.sprintf "cd 3rd-party/%s && ./configure " dir in
	    let configure = Cmd (S[Sh evil_cmd;
			                   A"--disable-bzip";
			                   A"--disable-zlib";
			                   (* A"--disable-shared"; *)
			                  ]) in
	    Seq[configure;]
      end;

    rule "make 3rd-party"
      ~dep:"3rd-party/%(dir).configured"
      ~stamp:"3rd-party/%(dir).make"
      begin fun env _build ->
	    let dir = env "%(dir)" in
	    let () = Log.dprintf 0 "make 3rd-party %s" dir in
	    let make =
	      Printf.sprintf "cd 3rd-party/%s && make" dir in
	    let cmd = Cmd(Sh make) in
	    Seq[cmd]
      end;

    flag ["ocaml";"compile";] (S[A"-thread"]);
    dep ["compile";"ocaml"][tc_home ^ ".make"];
    flag ["link"] (S[A"-thread"]);
    (* we need libotc.a *)
    dep ["ocaml";"link";"is_main"]["libotc.a"];
    (* 
       that triggers the compilation of otc_wrapper.c 
       for which we need to supply tokyo cabinet headers
    *)
    flag ["compile"; "c";"file:otc_wrapper.c"] (S[A"-ccopt";A( "-I" ^ tc_home);]);
    (* 
       at this point, we'll get an error while linking:
       otc.cmo:
       external function 'bdb_record' is not available. 
    *)
    flag ["ocaml";"byte";"link"] (S[A"-custom"]);
    flag ["ocaml";"link"](S[
      A"-cclib";A"-L.";
      A"-cclib";A"-lotc";
      A"-cclib"; A"-ltokyocabinet";
      A"-cclib"; A("-L" ^ tc_home);
    ]);
  | _ -> ()
