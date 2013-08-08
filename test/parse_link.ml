open Types

let string_of_link l =
  Printf.sprintf "%s" l.link_title

let main () =
  if Array.length Sys.argv < 2 then
    Printf.fprintf stderr "Usage: %s link.json\n%!" Sys.argv.(0)
  else
    let oc = open_in Sys.argv.(1) in
    let decoder = Jsonm.decoder (`Channel oc) in
    let stringmap = stringmap_of_lexemes decoder in
    let _ = Printf.printf "Keys found: "; StringMap.iter (fun k _ -> Printf.printf "%s " k) stringmap; print_endline "" in
    let link = link_of_stringmap stringmap in
    Printf.printf "%s\n%!" (string_of_link link)

let _ = main ()
