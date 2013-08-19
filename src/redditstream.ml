open Cohttp

module C = Cohttp_lwt_unix.Client
module B = Cohttp_lwt_body
module YB = Yojson.Basic

let (>>=) = Lwt.bind
let (|>) x f = f x
let user_agent = "athanor/0.1 by vbmithr"
let daemonize = ref false

let string_of_link json =
  let open Unix in
  let date = json |> YB.Util.member "created_utc" |> YB.Util.to_number |> gmtime in
  let title = json |> YB.Util.member "title" |> YB.Util.to_string in
  Printf.sprintf "%d-%02d-%02d %02d:%02d:%02d %s"
    (date.tm_year+1900) date.tm_mon date.tm_mday
    date.tm_hour date.tm_min date.tm_sec
    title

let sanitize_json_object f = function
  | `Assoc assocs -> `Assoc (YB.Util.filter_map f assocs)
  | _ -> raise (Invalid_argument "not a JSON object")

let sanitize_link kv = match fst kv with
  | "id" -> Some ("_id", snd kv)
  | "title" -> Some kv
  | "url" -> Some kv
  | "author" -> Some kv
  | "created_utc" -> Some kv
  | "downs" -> Some kv
  | "ups" -> Some kv
  | "score" -> Some kv
  | "selftext" -> Some kv
  | "num_comments" -> Some kv
  | "subreddit_id" -> Some kv
  | "permalink" -> Some kv
  | _ -> None

let decode_page ?(get_all = false) h json =
  let last_link_id =
    try json |> YB.Util.member "data" |> YB.Util.member "after" |> YB.Util.to_string
    with YB.Util.Type_error _ -> "" in
  if last_link_id = "" then
    Lwt.return None
  else
    let links = json |> YB.Util.member "data" |> YB.Util.member "children" |> YB.Util.to_list in
    let links = List.map (YB.Util.member "data") links in
    let links = List.map (sanitize_json_object sanitize_link) links in
    Lwt_list.map_s
      (fun link ->
         if not !daemonize then Printf.printf "%s\n%!" (string_of_link link);
         Couchdb.Doc.add h "reddit" link)
      links >>= fun m ->
    let nb_links, nb_new_links =
      List.fold_left (fun (a,b) l ->
          match l with `Failure _ -> (a+1, b) | _ -> (a+1, b+1)) (0,0) m in
    Printf.printf "%d new articles inserted in the database out of %d retrieved.\n%!"
      nb_new_links nb_links;
    if (nb_links <> nb_new_links) && not get_all
    then Lwt.return None
    else Lwt.return (Some last_link_id)

let main ?after ?before ~db_uri ~freq ~limit ~get_all subreddit =
  (* Creates the "reddit" DB. *)
  Couchdb.handle ~uri:db_uri ()
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
    | Some (resp, body) ->
      B.string_of_body body >>= fun bs ->
      YB.from_string bs |>
      decode_page ~get_all h >>= (function
          | None ->
            Printf.printf "No new links, waiting for %.0f seconds before retrying...\n%!" freq;
            Lwt_unix.sleep freq >>= fun () -> fetch_and_decode base_uri
          | Some last_id ->
            let uri = Uri.add_query_param' base_uri ("after", last_id) in
            Lwt_unix.sleep 1.0 >>= fun () ->
            fetch_and_decode uri)
    | None ->
      Printf.printf "Connection to reddit failed. Retrying in %.0f seconds.\n%!" freq;
      Lwt_unix.sleep freq >>= fun () ->
      fetch_and_decode uri
  in
  let rec forever () =
    Lwt.catch
      (fun () -> fetch_and_decode init_uri)
      (fun exn -> Lwt_io.printf "Caught unhandled exception %s\n"
          (Printexc.to_string exn)) >>= fun () ->
    forever () in
  forever ()

let _ =
  let open Arg in
  let db_uri = ref "http://localhost:5984" in
  let subreddit = ref "" in
  let limit = ref 25 in
  let after = ref None in
  let freq = ref 600.0 in
  let get_all = ref false in
  let speclist = align [
      "--db-uri", Set_string db_uri, "<string> URI of the CouchDB database in use (default: http://localhost:5984).";
      "--limit", Set_int limit, "<int> Number of links returned by one API call (default: 25).";
      "--all", Set get_all, " Do not stop fetching links when CouchDB says it is already known.";
      "--after", String (fun id -> after := Some id), "<link_id> Get links posted prior <link_id> (default: most recent link).";
      "--freq", Set_float freq, "<float> Number of seconds between each API call (default: 600).";
      "--daemon", Set daemonize, " Start the program as a daemon."
    ] in
  let anon_fun s = subreddit := s in
  let usage_msg = "Usage: " ^ Sys.argv.(0) ^ " <options> subreddit\nOptions are:" in
  parse speclist anon_fun usage_msg;
  if !daemonize then Lwt_daemon.daemonize ();
  Lwt_main.run
    (main
       ~db_uri:!db_uri
       ~freq:!freq
       ?after:!after
       ~limit:!limit
       ~get_all:!get_all
       !subreddit)
