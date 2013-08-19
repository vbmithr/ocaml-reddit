type kind =
  | Comment
  | Account
  | Link
  | Message
  | Subreddit
  | Award
  | PromoCampaign

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
  if String.length guid >= 3 then
    (kind_of_prefix (String.sub guid 0 2)), String.sub guid 3 (String.length guid - 3)
  else
    raise (Invalid_argument "guid")

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


