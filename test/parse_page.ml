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

let decode_page h decoder =
  let maps = stringmaps_of_page decoder in
  if maps = [] then Lwt.return None else
    let links = List.map link_of_stringmap maps in
    let last_link = List.hd links in
    let last_id = "t3_" ^ last_link.link_id in
    (* Adding the results in the DB. *)
    let links = List.rev links in
    List.iter (fun link -> Printf.printf "%s\n%!" (string_of_link link)) links;
    Lwt_list.map_p
      (fun link -> Couchdb.Doc.add h "reddit" (json_of_link link)) links
    >>= fun _ -> Lwt.return (Some last_id)

let main ?after ?before ?(limit="100") subreddit =
  (* Creates the "reddit" DB. *)
  let h = Couchdb.handle () in
  Lwt.catch
    (fun () -> Couchdb.DB.create h "reddit")
    (fun _ -> Lwt.return `Ok)
  >>= fun _ ->
  let base_uri = Uri.of_string ("http://www.reddit.com/r/" ^ subreddit ^ "/new.json") in
  let base_uri = Uri.add_query_param' base_uri ("limit", limit) in
  let init_uri = match after with
    | None -> base_uri
    | Some id -> Uri.add_query_param' base_uri ("after", id)
  in
  let headers = Header.init_with "User-Agent" user_agent in
  let rec fetch_and_decode uri =
    C.get ~headers uri >>= function
    | Some (resp, body) -> B.string_of_body body >>= fun body ->
      let decoder = Jsonm.decoder (`String body) in
      Lwt.catch (fun () -> decode_page h decoder >>= function
        | None -> Lwt.return ()
        | Some last_id ->
          let uri = Uri.add_query_param' base_uri ("after", last_id) in
          Lwt_unix.sleep 2.0 >>= fun () ->
          fetch_and_decode uri)
        (fun _ -> Lwt.return ())
    | None -> Lwt.return () in
  fetch_and_decode init_uri

let print_usage () = Printf.fprintf stderr "Usage: %s subreddit [limit] [after]\n%!" Sys.argv.(0)

let _ =
  match Array.length Sys.argv with
  | 1 -> print_usage ()
  | 2 -> Lwt_main.run (main Sys.argv.(1))
  | 3 -> Lwt_main.run (main ~limit:Sys.argv.(2) Sys.argv.(1))
  | _ -> Lwt_main.run (main ~limit:Sys.argv.(2) ~after:Sys.argv.(3) Sys.argv.(1))
