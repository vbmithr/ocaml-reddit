open Types
open Cohttp

module C = Cohttp_lwt_unix.Client
module B = Cohttp_lwt_body

let (>>=) = Lwt.bind
let user_agent = "athanor/0.1 by vbmithr"

let string_of_link l =
  let open Unix in
  let date = gmtime l.link_created_utc in
  Printf.sprintf "%d-%02d-%02d %02d:%02d:%02d %s"
    (date.tm_year+1900) date.tm_mon date.tm_mday
    date.tm_hour date.tm_min date.tm_sec
    l.link_title

let decode_page decoder =
  let maps = stringmaps_of_page decoder in
  (* let _ = Printf.printf "Keys found: "; StringMap.iter (fun k _ -> Printf.printf "%s " k) stringmap; print_endline "" in *)
  let links = List.map link_of_stringmap maps in
  let last_link = List.hd links in
  let last_id = "t3_" ^ last_link.link_id in
  List.iter (fun link -> Printf.printf "%s\n%!" (string_of_link link)) (List.rev links);
  last_id

let main () =
  let init_uri = Uri.of_string ("http://www.reddit.com/r/" ^ Sys.argv.(1) ^ "/new.json") in
  let init_uri = Uri.add_query_param' init_uri ("limit", Sys.argv.(2)) in
  let headers = Header.init_with "User-Agent" user_agent in
  let rec fetch_and_decode uri =
    Printf.printf "\nUri: %s\n%!" (Uri.to_string uri);
    C.get ~headers uri >>= function
    | Some (resp, body) -> B.string_of_body body >>= fun body ->
      let decoder = Jsonm.decoder (`String body) in
      let last_id = decode_page decoder in
      let uri = Uri.add_query_param' init_uri ("after", last_id) in
      Lwt_unix.sleep 2.0 >>= fun () ->
      fetch_and_decode uri
    | None -> Lwt.return () in
  fetch_and_decode init_uri

let _ =
  if Array.length Sys.argv < 2 then
    Printf.fprintf stderr "Usage: %s page.json\n%!" Sys.argv.(0)
  else
    Lwt_main.run (main ())
