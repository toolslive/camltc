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

open Ocamlbuild_pack
open Ocamlbuild_plugin
open Unix

let run_and_read = Ocamlbuild_pack.My_unix.run_and_read
let tc_home ="3rd-party/tokyocabinet"
let tc_source = "../" ^ tc_home

let run_cmd cmd =
  try
    String.trim(run_and_read cmd)
  with Failure _ -> "Not available"

let git_revision = run_cmd "git describe --all --long --always --dirty"
let tag_version  = run_cmd "git describe --tags --exact-match"
let branch_version = run_cmd "git describe --all"
let machine = run_cmd "uname -mnrpio"
let dependencies = [
  Printf.sprintf "Incubaid's tokyocabinet fork: %s"
    (run_cmd "git submodule status 3rd-party/tokyocabinet")
]
let time =
  let tm = Unix.gmtime (Unix.time()) in
  Printf.sprintf "%02d/%02d/%04d %02d:%02d:%02d UTC"
    (tm.tm_mday) (tm.tm_mon + 1) (tm.tm_year + 1900)
    tm.tm_hour tm.tm_min tm.tm_sec

let make_version _ _ =
  let cmd =
    let template = "let git_revision = %S\n" ^^
                     "let compile_time = %S\n" ^^
                     "let machine = %S\n" ^^
                     "let major = %i\n" ^^
                     "let minor = %i\n" ^^
                     "let patch = %i\n" ^^
                     "let dependencies = %S\n"
    in
    let major,minor,patch =
      try
        Scanf.sscanf tag_version "camltc-%i.%i.%i" (fun ma mi p -> (ma,mi,p))
      with _ ->
        try Scanf.sscanf branch_version "heads/%i.%i" (fun ma mi -> (ma,mi,-1))
        with _ ->
          (* This one matches what's on Jenkins slaves *)
          try Scanf.sscanf branch_version "remotes/origin/%i.%i"
                (fun ma mi -> (ma, mi, -1))
          with _ -> (-1,-1,-1)
    in
    Printf.sprintf template git_revision time machine major minor patch
      (String.concat "\\n" dependencies)
  in
  Cmd (S [A "echo"; Quote(Sh cmd); Sh ">"; P "camltc_version.ml"])



let _ = dispatch & function
  | After_rules ->
    rule "camltc_version.ml" ~prod: "camltc_version.ml" make_version;

    rule "configure 3rd-party"
      ~dep:"3rd-party/%(dir)/configure"
      ~stamp:"3rd-party/%(dir).configured"
      begin fun env _build ->
        let dir =  env "%(dir)" in
        let configure = env "../../../3rd-party/%(dir)/configure" in
        let () = Log.dprintf 0 "configuring %s" dir in
        let evil_cmd =
          Printf.sprintf "mkdir -p 3rd-party/%s && cd 3rd-party/%s && %s" dir dir configure in
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
          Printf.sprintf "make -C 3rd-party/%s" dir in
        let cmd = Cmd(Sh make) in
        Seq[cmd]
      end;

    dep ["compile";"ocaml"][tc_home ^ ".make"];
    (* we need libotc.a *)
    dep ["ocaml";"link";"is_main"]["libotc.a"];
    (*
      that triggers the compilation of otc_wrapper.c
      for which we need to supply tokyo cabinet headers
    *)
    flag ["compile"; "c";"file:otc_wrapper.c"]
      (S[A"-ccopt";A( "-I" ^ tc_source);
         A"-ccopt"; A"-O2";
        ]);
    (*
      at this point, we'll get an error while linking:
      otc.cmo:
      external function 'bdb_record' is not available.
    *)
    flag ["ocaml";"byte";"link"] (S[A"-dllib";A"-lotc";A"-custom"]);
    flag ["ocaml";"link"](S[
      A"-cclib";A"-L.";
      A"-cclib";A"-lotc";
      A"-cclib"; A"-ltokyocabinet";
      A"-cclib"; A("-L" ^ tc_home);
    ]);
  | _ -> ()
