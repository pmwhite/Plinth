let () =
  let s = In_channel.input_all In_channel.stdin in
  match Plinth.type_of s with
  | exception Plinth.User_error msg -> Printf.eprintf "%s\n" msg
  | s -> print_endline s
;;
