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
  let maps = try stringmaps_of_page decoder with _ -> [] in
  if maps = [] then Lwt.return None else
    let links = List.map link_of_stringmap maps in
    let last_link = List.hd links in
    let last_id = "t3_" ^ last_link.link_id in
    (* Adding the results in the DB. *)
    let links = List.rev links in
    Lwt_list.map_s
      (fun link ->
         Printf.printf "%s\n%!" (string_of_link link);
         Couchdb.Doc.add h "reddit" (json_of_link link)) links
    >>= fun m -> if List.exists (function `Error _ -> true | _ -> false) m
    then Lwt.return None
    else Lwt.return (Some last_id)

let main ?db_uri ?(freq=600.0) ?after ?before ?(limit=100) subreddit =
  (* Creates the "reddit" DB. *)
  Couchdb.handle ?uri:db_uri ()
  >>= fun h -> Couchdb.DB.create h "reddit"
  >>= fun _ ->
  let base_uri = Uri.of_string ("http://www.reddit.com/r/" ^ subreddit ^ "/new.json") in
  let base_uri = Uri.add_query_param' base_uri ("limit", string_of_int limit) in
  let init_uri = match after with
    | None -> base_uri
    | Some id -> Uri.add_query_param' base_uri ("after", id)
  in
  let headers = Header.init_with "User-Agent" user_agent in
  let rec fetch_and_decode uri =
    C.get ~headers uri >>= function
    | Some (resp, body) -> B.string_of_body body >>= fun body ->
      let decoder = Jsonm.decoder (`String body) in
      (decode_page h decoder >>= function
        | None ->
          Printf.printf "\nNo new links, waiting for %.0f seconds before retrying...\n%!" freq;
          Lwt_unix.sleep freq >>= fun () -> fetch_and_decode base_uri
        | Some last_id ->
          let uri = Uri.add_query_param' base_uri ("after", last_id) in
          Lwt_unix.sleep 2.0 >>= fun () ->
          fetch_and_decode uri)
    | None ->
      Printf.printf "Connection to reddit failed. Retrying in %.0f seconds.\n%!" freq;
      Lwt_unix.sleep freq >>= fun () -> fetch_and_decode uri in
  fetch_and_decode init_uri

let _ =
  let open Arg in
  let db_uri = ref "http://localhost:5984" in
  let subreddit = ref "" in
  let limit = ref 100 in
  let after = ref None in
  let freq = ref 600.0 in
  let speclist = align [
      "--db_uri", Set_string db_uri, "<string> URI of the CouchDB database in use (default: http://localhost:5984).";
      "--limit", Set_int limit, "<int> Number of links returned by one API call (default: 100).";
      "--after", String (fun id -> after := Some id), "<link_id> Get links posted prior <link_id> (default: most recent link).";
      "--freq", Set_float freq, "<float> Number of seconds between each API call (default: 600)."
    ] in
  let anon_fun s = subreddit := s in
  let usage_msg = "Usage: " ^ Sys.argv.(0) ^ " [--db_uri <string>] [--limit <int>] [--after <link_id>] [--freq <float>] subreddit" in
  parse speclist anon_fun usage_msg;
  Lwt_main.run (main ~db_uri:!db_uri ~freq:!freq ?after:!after ~limit:!limit !subreddit)
