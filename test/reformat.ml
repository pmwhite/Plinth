let () =
  let s = In_channel.input_all In_channel.stdin in
  match Plinth.reformat s with
  | exception Plinth.User_error msg -> Printf.eprintf "%s" msg
  | s ->
    (match Plinth.reformat s with
     | exception Plinth.User_error msg -> Printf.eprintf "Roundtrip failed: %s\n%s" msg s
     | roundtripped ->
       if String.equal s roundtripped
       then print_endline s
       else Printf.eprintf "The printer and parser did not roundtrip")
