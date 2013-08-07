open Types

let main () =
  if Array.length Sys.argv < 2 then
    Printf.fprintf stderr "Usage: %s page.json\n%!" Sys.argv.(0)
  else
    let oc = open_in Sys.argv.(1) in
    let decoder = Jsonm.decoder (`Channel oc) in
    let maps = stringmaps_of_page decoder in
    (* let _ = Printf.printf "Keys found: "; StringMap.iter (fun k _ -> Printf.printf "%s " k) stringmap; print_endline "" in *)
    let links = List.map link_of_stringmap maps in
    List.iter (fun link -> Printf.printf "%s\n%!" (string_of_link link)) links

let _ = main ()
