open Otc

module Prefix_otc = struct

  type t = Bdb.bdb

  let get bdb prefix key =
    Lwt.catch (fun () ->
    let value = Bdb.get bdb (prefix ^ key) in
    (*let _ = log "GET %d" (String.length value) in*)
      Lwt.return value)
      (fun e -> Lwt.fail e)


  let put bdb prefix key value =
    (*let _ = log "PUT %d" (String.length value) in*)
    Lwt.catch (fun () ->
      let () = Bdb.put bdb (prefix ^ key) value in
      Lwt.return ()
    ) (fun e -> Lwt.fail e)

  let out bdb prefix key =
    Lwt.catch (fun () ->
      let () = Bdb.out bdb (prefix ^ key) in
      Lwt.return ()
    ) (fun e -> Lwt.fail e)

  let fold (f:string -> string -> 'c -> 'c) (bdb:t) (prefix:string) (init:'c) =
    Lwt.catch (fun () ->
      let f' a key = f key (Bdb.get bdb key) a in
      let (keys:string array) = Bdb.prefix_keys bdb prefix (-1) in
      let x = Array.fold_left f' init keys in
      Lwt.return x
    ) (fun e -> Lwt.fail e)

  let iter (f:string -> string -> unit) (bdb:t) (prefix:string) =
    fold (fun k v _ -> f k v) bdb prefix ()

  let all_keys bdb prefix =
    fold (fun k v init -> k::init) bdb prefix []

  let all_values bdb prefix =
    fold (fun k v init -> v::init) bdb prefix []

      (* TODO: add more prefixed Otc methods as needed *)

end
