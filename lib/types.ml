type kind = Comment | Account | Link | Message | Subreddit | Award | PromoCampaign

let prefix_of_kind = function
  | Comment -> "t1"
  | Account -> "t2"
  | Link    -> "t3"
  | Message -> "t4"
  | Subreddit -> "t5"
  | Award -> "t6"
  | PromoCampaign -> "t8"

let kind_of_prefix = function
  | "t1" -> Comment
  | "t2" -> Account
  | "t3" -> Link
  | "t4" -> Message
  | "t5" -> Subreddit
  | "t6" -> Award
  | "t8" -> PromoCampaign
  | _    -> raise (Invalid_argument "prefix")

type uid = string

type guid = kind * uid

let string_of_guid (k, u) = prefix_of_kind k ^ "_" ^ u

let guid_of_string guid =
  try
    (kind_of_prefix (String.sub guid 0 2)), String.sub guid 3 (String.length guid - 3)
  with _ -> raise (Invalid_argument "guid")

type link =
  {
    link_id: uid;
    link_title: string;
    link_url: string;
    link_author: string;
    link_created_utc: float;
    link_downs: int;
    link_ups: int;
    link_score: int;
    link_selftext: string;
    link_num_comments: int;
    link_subreddit_id: guid;
    link_permalink: string;
  }

let json_of_link l =
  `Assoc [
    "_id", `String l.link_id;
    "title", `String l.link_title;
    "url", `String l.link_url;
    "author", `String l.link_author;
    "created_utc", `Float l.link_created_utc;
    "downs", `Int l.link_downs;
    "ups", `Int l.link_ups;
    "score", `Int l.link_score;
    "selftext", `String l.link_selftext;
    "num_comments", `Int l.link_num_comments;
    "subreddit_id", `String (string_of_guid l.link_subreddit_id);
    "permalink", `String l.link_permalink;
  ]

module StringSet = Set.Make(String)
module StringMap = Map.Make(String)

let string_of_lexeme = function
  | `String str -> str
  | _ -> raise (Invalid_argument "Not a `String")

let float_of_lexeme = function
  | `Float fl -> fl
  | _ -> raise (Invalid_argument "Not a `Float")

let int_of_lexeme = function
  | `Float fl -> int_of_float fl
  | _ -> raise (Invalid_argument "Not a `Float")

let link_of_stringmap map =
  {
    link_id= string_of_lexeme (StringMap.find "id" map);
    link_title= string_of_lexeme (StringMap.find "title" map);
    link_url= string_of_lexeme (StringMap.find "url" map);
    link_author= string_of_lexeme (StringMap.find "author" map);
    link_created_utc= float_of_lexeme (StringMap.find "created_utc" map);
    link_downs= int_of_lexeme (StringMap.find "downs" map);
    link_ups= int_of_lexeme (StringMap.find "ups" map);
    link_score= int_of_lexeme (StringMap.find "score" map);
    link_selftext= string_of_lexeme (StringMap.find "selftext" map);
    link_num_comments= int_of_lexeme (StringMap.find "num_comments" map);
    link_subreddit_id= guid_of_string (string_of_lexeme (StringMap.find "subreddit_id" map));
    link_permalink= string_of_lexeme (StringMap.find "permalink" map)
  }

let interesting_names =
  List.fold_left
    (fun acc e -> StringSet.add e acc)
    StringSet.empty
    ["id"; "title"; "url"; "author"; "created_utc"; "downs"; "ups"; "score";
     "selftext"; "num_comments"; "subreddit_id"; "permalink"]

let stringmap_of_lexemes decoder =
  let rec inner level map key =
    match Jsonm.decode decoder with
    | `Error err -> failwith "invalid json input"
    | `End -> map
    | `Await -> assert false
    | `Lexeme l -> (match l with
        | `Os -> inner (level+1) map key
        | `Oe -> if level > 0 then inner (level-1) map key else map
        | `As | `Ae -> failwith "stringmap_of_lexemes"
        | `Name str ->
          if StringSet.mem str interesting_names then inner level map (Some str)
          else inner level map None
        | l ->
          (match key with
           | Some k -> inner level (StringMap.add k l map) None
           | None   -> inner level map None))
  in inner 0 StringMap.empty None

let stringmaps_of_page decoder =
  let rec inner ns maps = match Jsonm.decode decoder with
    | `End -> maps
    | `Error err -> failwith "invalid json input"
    | `Await -> assert false
    | `Lexeme l -> (match l with
        | `As | `Ae -> inner ns maps
        | `Os ->
          if ns = ["data"; "children"; "data"]
          then inner ns ((stringmap_of_lexemes decoder)::maps)
          else inner ns maps
        | `Oe -> inner (List.tl ns) maps
        | `Name str -> inner (str::ns) maps
        | l -> inner (List.tl ns) maps
      )
  in inner [] []
