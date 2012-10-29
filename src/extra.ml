
let eq_string str i1 i2 =
  let msg = Printf.sprintf "%s expected:%s actual:%s" str i1 i2 in
  OUnit.assert_equal ~msg i1 i2

let eq_int str i1 i2 =
  let msg = Printf.sprintf "%s expected:%d actual:%d" str i1 i2 in
  OUnit.assert_equal ~msg i1 i2

let eq_conv conv str i1 i2 =
  let c1 = conv i1 and c2 = conv i2 in
  let msg = Printf.sprintf "%s expected:%s actual:%s" str c1 c2 in
  OUnit.assert_equal ~msg i1 i2

open Lwt

let lwt_bracket setup testcase teardown () =
  let try_lwt_ f =
    Lwt.catch f (fun exn -> Lwt.fail exn)
  in
  Lwt_main.run
    begin
    try_lwt_ setup >>= fun x ->
    try_lwt_ (fun () ->
      Lwt.finalize (fun () -> testcase x)
	(fun () -> teardown x)
    ) >>= fun () ->
    Lwt.return ()
    end

let lwt_test_wrap testcase =
  let setup = Lwt.return and teardown _ = Lwt.return () in
  lwt_bracket setup testcase teardown

let timeout_thread timeout_sec f =
  let sleep_sec = float_of_int (timeout_sec) in
  let t =
    begin
      Lwt_unix.sleep sleep_sec >>= fun () ->
      f ()
    end in
  let () = Lwt.ignore_result t in
  t

