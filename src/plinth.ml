module String_map = Map.Make (String)
module Int_map = Map.Make (Int)

type type_ =
  | Unknown
  | Unknown_bits
  | Bits of int
  | Pointer of type_
  | Function of
      { args : type_ list
      ; return : type_
      }

type expr =
  | Identifier of string
  | Integer of
      { data : string
      ; mutable size : int
      }
  | Comment of
      { text : string
      ; expr : expr
      }
  | Let of
      { binding : binding
      ; body : expr
      }
  | Rec of
      { bindings : binding list
      ; body : expr
      }
  | Match of
      { expr : expr
      ; cases : (string * expr) list
      }
  | Fn of
      { arg_names : (string * type_ option) list
      ; body : expr
      }
  | Call of
      { fn : expr
      ; args : expr list
      }

and binding =
  { name : string
  ; type_ : type_ option
  ; expr : expr
  }

exception User_error of string

let user_error fmt = Printf.ksprintf (fun msg -> raise_notrace (User_error msg)) fmt

type parse_state =
  { input : string
  ; input_len : int
  ; mutable index : int
  }

let partial_token_buffer = Buffer.create 1024

let parse_characters_satisfying
  (ps : parse_state)
  ~(first : char -> bool)
  ~(rest : char -> bool)
  ~(fail : unit -> unit)
  : string
  =
  let rec iter i =
    if i < ps.input_len
    then (
      let c = ps.input.[i] in
      if rest c
      then (
        Buffer.add_char partial_token_buffer c;
        iter (i + 1))
      else i)
    else i
  in
  let i = ps.index in
  let i =
    if i < ps.input_len
    then (
      let c = ps.input.[i] in
      if first c
      then (
        Buffer.add_char partial_token_buffer c;
        iter (i + 1))
      else (
        fail ();
        i))
    else (
      fail ();
      i)
  in
  ps.index <- i;
  let result = Buffer.contents partial_token_buffer in
  Buffer.clear partial_token_buffer;
  result
;;

let parse_symbol (ps : parse_state) : string =
  parse_characters_satisfying
    ps
    ~first:(function
      | 'a' .. 'z' | '_' -> true
      | _ -> false)
    ~rest:(function
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
      | _ -> false)
    ~fail:(fun () -> user_error "Parse error: expected symbol char.")
;;

let parse_integer (ps : parse_state) : string =
  parse_characters_satisfying
    ps
    ~first:(function
      | '+' | '-' | '0' .. '9' -> true
      | _ -> false)
    ~rest:(function
      | '0' .. '9' -> true
      | _ -> false)
    ~fail:(fun () -> user_error "Parse error: expected integer char.")
;;

let parse_size (ps : parse_state) : int =
  let rec iter acc =
    if ps.index < ps.input_len
    then (
      let c = ps.input.[ps.index] in
      match c with
      | '0' .. '9' ->
        ps.index <- ps.index + 1;
        iter ((acc * 10) + (int_of_char c - int_of_char '0'))
      | _ -> acc)
    else acc
  in
  if ps.index < ps.input_len
  then (
    let c = ps.input.[ps.index] in
    match c with
    | '0' .. '9' ->
      ps.index <- ps.index + 1;
      iter (int_of_char c - int_of_char '0')
    | _ -> user_error "Parse error: expected digit char.")
  else user_error "Parse error: expected digit char."
;;

let parse_whitespace (ps : parse_state) : unit =
  let rec iter i =
    if i < ps.input_len
    then (
      let c = ps.input.[i] in
      match c with
      | ' ' | '\n' | '\t' | '\r' -> iter (i + 1)
      | _ -> i)
    else i
  in
  ps.index <- iter ps.index
;;

let parse_equals (ps : parse_state) : unit =
  if ps.index < ps.input_len && Char.equal ps.input.[ps.index] '='
  then ps.index <- ps.index + 1
  else user_error "Parse error: expected '='"
;;

let parse_open_brace (ps : parse_state) : unit =
  if ps.index < ps.input_len && Char.equal ps.input.[ps.index] '{'
  then ps.index <- ps.index + 1
  else user_error "Parse error: expected '{'"
;;

let parse_arrow (ps : parse_state) : unit =
  if ps.index + 1 < ps.input_len
     && Char.equal ps.input.[ps.index] '-'
     && Char.equal ps.input.[ps.index + 1] '>'
  then ps.index <- ps.index + 2
  else user_error "Parse error: expected '->'"
;;

let parse_single_char (ps : parse_state) (c : char) : unit =
  if ps.index < ps.input_len && Char.equal ps.input.[ps.index] c
  then ps.index <- ps.index + 1
  else user_error "Parse error: expected '%c'" c
;;

let parse_open_paren ps = parse_single_char ps '('
let parse_close_paren ps = parse_single_char ps ')'
let parse_comma ps = parse_single_char ps ','

let parse_args (type a) (ps : parse_state) ~(f : parse_state -> a) : a list =
  let rec iter () =
    if ps.index < ps.input_len && Char.equal ps.input.[ps.index] ')'
    then (
      ps.index <- ps.index + 1;
      [])
    else (
      parse_comma ps;
      parse_whitespace ps;
      let arg = f ps in
      parse_whitespace ps;
      let rest = iter () in
      arg :: rest)
  in
  if ps.index < ps.input_len && Char.equal ps.input.[ps.index] ')'
  then (
    ps.index <- ps.index + 1;
    [])
  else (
    let arg = f ps in
    parse_whitespace ps;
    let rest = iter () in
    arg :: rest)
;;

let rec parse_type (ps : parse_state) =
  if ps.index < ps.input_len
  then (
    let c = ps.input.[ps.index] in
    match c with
    | '0' .. '9' -> Bits (parse_size ps)
    | 'a' .. 'z' | '_' ->
      let symbol = parse_symbol ps in
      (match symbol with
       | "ptr" ->
         parse_whitespace ps;
         parse_open_paren ps;
         parse_whitespace ps;
         let inner = parse_type ps in
         parse_whitespace ps;
         parse_close_paren ps;
         Pointer inner
       | "fn" ->
         parse_whitespace ps;
         parse_open_paren ps;
         parse_whitespace ps;
         let args = parse_args ps ~f:parse_type in
         parse_whitespace ps;
         let return = parse_type ps in
         Function { args; return }
       | _ -> user_error "Parse error: type")
    | _ -> user_error "Parse error: expected type")
  else assert false
;;

let parse_name_and_type (ps : parse_state) : string * type_ option =
  let name = parse_symbol ps in
  parse_whitespace ps;
  let type_ =
    if ps.index < ps.input_len && Char.equal ps.input.[ps.index] ':'
    then (
      ps.index <- ps.index + 1;
      parse_whitespace ps;
      Some (parse_type ps))
    else None
  in
  name, type_
;;

let parse_comment (ps : parse_state) : string =
  parse_characters_satisfying
    ps
    ~first:(function
      | '\n' -> false
      | _ -> true)
    ~rest:(function
      | '\n' -> false
      | _ -> true)
    ~fail:(fun () -> ())
;;

let rec parse_expr (ps : parse_state) : expr =
  let i = ps.index in
  if i < ps.input_len
  then (
    let c = ps.input.[i] in
    match c with
    | '+' | '-' | '0' .. '9' -> Integer { data = parse_integer ps; size = 0 }
    | '/' ->
      ps.index <- ps.index + 1;
      let text = parse_comment ps in
      parse_whitespace ps;
      let expr = parse_expr ps in
      Comment { text; expr }
    | 'a' .. 'z' | '_' ->
      let symbol = parse_symbol ps in
      (match symbol with
       | "let" ->
         parse_whitespace ps;
         let binding = parse_binding ps in
         parse_whitespace ps;
         let body = parse_expr ps in
         Let { binding; body }
       | "rec" ->
         parse_whitespace ps;
         let binding = parse_binding ps in
         parse_whitespace ps;
         let rest = parse_rec_bindings ps in
         parse_whitespace ps;
         let body = parse_expr ps in
         Rec { bindings = binding :: rest; body }
       | "match" ->
         parse_whitespace ps;
         let expr = parse_expr ps in
         parse_whitespace ps;
         parse_open_brace ps;
         parse_whitespace ps;
         let cases = parse_cases ps in
         Match { expr; cases }
       | "fn" ->
         parse_whitespace ps;
         parse_open_paren ps;
         parse_whitespace ps;
         let arg_names = parse_args ps ~f:parse_name_and_type in
         parse_whitespace ps;
         let body = parse_expr ps in
         Fn { arg_names; body }
       | _ ->
         let expr = Identifier symbol in
         parse_and_attach_post_expr_operations ps expr)
    | _ -> user_error "Parse error: expected expression.")
  else user_error "Parse error: expected expression."

and parse_and_attach_post_expr_operations (ps : parse_state) (expr : expr) : expr =
  parse_whitespace ps;
  if ps.index < ps.input_len && Char.equal ps.input.[ps.index] '('
  then (
    ps.index <- ps.index + 1;
    parse_whitespace ps;
    let args = parse_args ps ~f:parse_expr in
    let expr = Call { fn = expr; args } in
    parse_and_attach_post_expr_operations ps expr)
  else expr

and parse_cases (ps : parse_state) : (string * expr) list =
  if ps.index < ps.input_len && Char.equal ps.input.[ps.index] '}'
  then (
    ps.index <- ps.index + 1;
    [])
  else (
    let integer = parse_integer ps in
    parse_whitespace ps;
    parse_arrow ps;
    parse_whitespace ps;
    let body = parse_expr ps in
    parse_whitespace ps;
    let rest = parse_cases ps in
    (integer, body) :: rest)

and parse_binding (ps : parse_state) : binding =
  let name, type_ = parse_name_and_type ps in
  parse_whitespace ps;
  parse_equals ps;
  parse_whitespace ps;
  let expr = parse_expr ps in
  { name; type_; expr }

and parse_rec_bindings (ps : parse_state) : binding list =
  if ps.index < ps.input_len && Char.equal ps.input.[ps.index] '&'
  then (
    ps.index <- ps.index + 1;
    parse_whitespace ps;
    let binding = parse_binding ps in
    parse_whitespace ps;
    let rest = parse_rec_bindings ps in
    binding :: rest)
  else []
;;

let parse_program (ps : parse_state) : expr =
  parse_whitespace ps;
  parse_expr ps
;;

type output =
  { mutable indent : int
  ; mutable at_start_of_line : bool
  ; buffer : Buffer.t
  }

let output_string (output : output) (s : string) : unit =
  if output.at_start_of_line
  then
    for _ = 0 to (output.indent * 2) - 1 do
      Buffer.add_char output.buffer ' '
    done;
  output.at_start_of_line <- false;
  Buffer.add_string output.buffer s
;;

let output_newline (output : output) =
  Buffer.add_char output.buffer '\n';
  output.at_start_of_line <- true
;;

let output_indent t = t.indent <- t.indent + 1
let output_dedent t = t.indent <- t.indent - 1

let iter_sep_by xs ~f ~sep =
  match xs with
  | [] -> ()
  | x :: xs ->
    f x;
    ListLabels.iter xs ~f:(fun x ->
      sep ();
      f x)
;;

let rec output_type (output : output) (type_ : type_) : unit =
  match type_ with
  | Unknown -> output_string output "unknown"
  | Unknown_bits -> output_string output "unknown_bits"
  | Bits n -> output_string output (Int.to_string n)
  | Pointer type_ ->
    output_string output "ptr(";
    output_type output type_;
    output_string output ")"
  | Function { args; return } ->
    output_string output "fn(";
    iter_sep_by
      args
      ~sep:(fun () -> output_string output ", ")
      ~f:(fun arg -> output_type output arg);
    output_string output ") ";
    output_type output return
;;

let rec is_multiline_expr (expr : expr) : bool =
  match expr with
  | Identifier _ | Integer _ -> false
  | Comment _ | Let _ | Rec _ -> true
  | Call { fn = _; args } -> ListLabels.exists args ~f:is_multiline_expr
  | Fn { arg_names = _; body } -> is_multiline_expr body
  | Match { expr; cases } ->
    is_multiline_expr expr
    ||
      (match cases with
      | [] -> false
      | _ :: _ -> true)
;;

let rec output_expr (output : output) (expr : expr) : unit =
  match expr with
  | Identifier identifier -> output_string output identifier
  | Integer { data; size = _ } -> output_string output data
  | Comment { text; expr } ->
    output_string output "/";
    output_string output text;
    output_newline output;
    output_expr output expr
  | Let { binding; body } ->
    output_string output "let ";
    output_binding output binding;
    output_newline output;
    output_expr output body
  | Rec { bindings; body } ->
    output_string output "rec ";
    iter_sep_by
      bindings
      ~sep:(fun () ->
        output_newline output;
        output_string output "& ")
      ~f:(fun binding -> output_binding output binding);
    output_newline output;
    output_expr output body
  | Match { expr; cases } ->
    output_string output "match";
    if is_multiline_expr expr
    then (
      output_newline output;
      output_indent output;
      output_expr output expr;
      output_dedent output;
      output_newline output;
      output_string output "{")
    else (
      output_string output " ";
      output_expr output expr;
      output_string output " {");
    output_indent output;
    ListLabels.iter cases ~f:(fun (integer, body) ->
      output_newline output;
      output_string output integer;
      output_string output " ->";
      if is_multiline_expr body
      then (
        output_newline output;
        output_indent output;
        output_expr output body;
        output_dedent output)
      else (
        output_string output " ";
        output_expr output body));
    (match cases with
     | [] -> ()
     | _ :: _ -> output_newline output);
    output_dedent output;
    output_string output "}"
  | Fn { arg_names; body } ->
    output_string output "fn(";
    iter_sep_by
      arg_names
      ~sep:(fun () -> output_string output ", ")
      ~f:(fun (arg_name, type_) -> output_name_and_type output arg_name type_);
    output_string output ")";
    if is_multiline_expr body
    then (
      output_newline output;
      output_indent output;
      output_expr output body;
      output_dedent output)
    else (
      output_string output " ";
      output_expr output body)
  | Call { fn; args } ->
    output_expr output fn;
    output_string output "(";
    if is_multiline_expr expr
    then (
      output_newline output;
      output_indent output;
      iter_sep_by
        args
        ~sep:(fun () ->
          output_string output ",";
          output_newline output)
        ~f:(fun arg -> output_expr output arg);
      output_dedent output)
    else
      iter_sep_by
        args
        ~sep:(fun () -> output_string output ", ")
        ~f:(fun arg -> output_expr output arg);
    output_string output ")"

and output_name_and_type (output : output) name type_ : unit =
  output_string output name;
  match type_ with
  | None -> ()
  | Some type_ ->
    output_string output " : ";
    output_type output type_

and output_binding (output : output) ({ name; type_; expr } : binding) : unit =
  output_name_and_type output name type_;
  output_string output " =";
  if is_multiline_expr expr
  then (
    output_newline output;
    output_indent output;
    output_expr output expr;
    output_dedent output)
  else (
    output_string output " ";
    output_expr output expr)
;;

let reformat input =
  let ps = { input; input_len = String.length input; index = 0 } in
  let expr = parse_program ps in
  let output = { indent = 0; at_start_of_line = false; buffer = Buffer.create 1024 } in
  output_expr output expr;
  Buffer.contents output.buffer
;;

type env_entry =
  { mutable type_ : type_
  ; mutable expr : (env * expr) option
  }

and env = env_entry String_map.t

let env_find (fn_env : env) (env : env) (name : string) : env_entry =
  match String_map.find_opt name env with
  | Some entry -> entry
  | None ->
    (match String_map.find_opt name fn_env with
     | Some entry -> entry
     | None -> user_error "Unbound name %s" name)
;;

exception Unify_error

let unify_error () = raise_notrace Unify_error

let rec unify_types (a : type_) (b : type_) : type_ =
  match a with
  | Unknown -> b
  | Unknown_bits ->
    (match b with
     | Unknown | Unknown_bits -> Unknown_bits
     | Bits b -> Bits b
     | Pointer _ | Function _ -> unify_error ())
  | Bits a ->
    (match b with
     | Unknown | Unknown_bits -> Bits a
     | Bits b -> if Int.equal a b then Bits a else unify_error ()
     | Pointer _ | Function _ -> unify_error ())
  | Pointer a ->
    (match b with
     | Unknown -> Pointer a
     | Pointer b -> Pointer (unify_types a b)
     | Unknown_bits | Bits _ | Function _ -> unify_error ())
  | Function a ->
    (match b with
     | Unknown -> Function a
     | Unknown_bits | Bits _ | Pointer _ -> unify_error ()
     | Function b ->
       let rec iter xs ys =
         match xs, ys with
         | [], [] -> []
         | [], _ :: _ | _ :: _, [] -> unify_error ()
         | x :: xs, y :: ys -> unify_types x y :: iter xs ys
       in
       Function { args = iter a.args b.args; return = unify_types a.return b.return })
;;

let unify_types (a : type_) (b : type_) : type_ =
  try unify_types a b with
  | Unify_error ->
    let output = { indent = 0; at_start_of_line = false; buffer = Buffer.create 1024 } in
    output_string output (Printf.sprintf "Type error: could not unify types.");
    output_newline output;
    output_indent output;
    output_type output a;
    output_newline output;
    output_type output b;
    output_dedent output;
    output_newline output;
    user_error "%s" (Buffer.contents output.buffer)
;;

let rec type_is_fully_known (type_ : type_) : bool =
  match type_ with
  | Unknown | Unknown_bits -> false
  | Bits _ -> true
  | Pointer type_ -> type_is_fully_known type_
  | Function { return; args } ->
    type_is_fully_known return && ListLabels.for_all ~f:type_is_fully_known args
;;

let args_and_return_of_function_type (type_ : type_) : type_ * type_ list =
  match type_ with
  | Unknown | Unknown_bits | Pointer _ | Bits _ -> assert false
  | Function fn_type -> fn_type.return, fn_type.args
;;

let rec infer_type (fn_env : env) (env : env) (expr : expr) (incoming : type_) : type_ =
  match expr with
  | Identifier name -> env_find_and_infer fn_env env name incoming
  | Integer i ->
    let type_ = unify_types incoming Unknown_bits in
    if type_is_fully_known type_
    then (
      (match type_ with
       | Bits size -> i.size <- size
       | Unknown | Unknown_bits | Pointer _ | Function _ -> assert false);
      type_)
    else user_error "Could not infer type of '%s' due to lack of information" i.data
  | Comment { text = _; expr } -> infer_type fn_env env expr incoming
  | Let { binding; body } ->
    let entry : env_entry =
      match binding.type_ with
      | Some type_ ->
        let type_ = infer_type fn_env env binding.expr type_ in
        { type_; expr = None }
      | None -> { type_ = Unknown; expr = Some (env, binding.expr) }
    in
    let env = String_map.add binding.name entry env in
    let fn_env =
      match binding.expr with
      | Fn _ -> String_map.add binding.name entry fn_env
      | Identifier _ | Integer _ | Comment _ | Let _ | Rec _ | Match _ | Call _ -> fn_env
    in
    infer_type fn_env env body incoming
  | Rec { bindings; body } ->
    let env =
      ListLabels.fold_left bindings ~init:env ~f:(fun env (binding : binding) ->
        let entry : env_entry =
          match binding.type_ with
          | Some type_ -> { type_; expr = None }
          | None -> { type_ = Unknown; expr = None }
        in
        String_map.add binding.name entry env)
    in
    let fn_env =
      ListLabels.fold_left bindings ~init:fn_env ~f:(fun fn_env (binding : binding) ->
        let entry = env_find fn_env env binding.name in
        entry.expr <- Some (env, binding.expr);
        match binding.expr with
        | Fn _ -> String_map.add binding.name entry fn_env
        | Identifier _ | Integer _ | Comment _ | Let _ | Rec _ | Match _ | Call _ ->
          fn_env)
    in
    ListLabels.iter bindings ~f:(fun (binding : binding) ->
      match binding.type_ with
      | Some type_ ->
        let (_ : type_) = env_find_and_infer fn_env env binding.name type_ in
        ()
      | None -> ());
    infer_type fn_env env body incoming
  | Match { expr; cases } ->
    let result =
      ListLabels.fold_left cases ~init:incoming ~f:(fun incoming (_, expr) ->
        infer_type fn_env env expr incoming)
    in
    let (_ : type_) = infer_type fn_env env expr Unknown_bits in
    result
  | Fn { arg_names; body } ->
    let incoming_return, incoming_args =
      let fn_type =
        Function
          { return = Unknown; args = ListLabels.map arg_names ~f:(fun _ -> Unknown) }
      in
      args_and_return_of_function_type (unify_types incoming fn_type)
    in
    let env =
      ListLabels.fold_left2
        incoming_args
        arg_names
        ~init:String_map.empty
        ~f:(fun env incoming_type (arg_name, annotation_type) ->
          let annotation_type =
            match annotation_type with
            | Some annotation_type -> annotation_type
            | None -> Unknown
          in
          let entry =
            { type_ = unify_types annotation_type incoming_type; expr = None }
          in
          String_map.add arg_name entry env)
    in
    let return = infer_type fn_env env body incoming_return in
    let args =
      ListLabels.map arg_names ~f:(fun (arg_name, _) ->
        env_find_and_infer fn_env env arg_name Unknown)
    in
    Function { args; return }
  | Call { fn; args } ->
    let fn_return, fn_args =
      let fn_type =
        let incoming =
          Function { return = incoming; args = ListLabels.map args ~f:(fun _ -> Unknown) }
        in
        infer_type fn_env env fn incoming
      in
      args_and_return_of_function_type fn_type
    in
    ListLabels.iter2 fn_args args ~f:(fun fn_arg arg ->
      let (_ : type_) = infer_type fn_env env arg fn_arg in
      ());
    unify_types fn_return incoming

and env_find_and_infer (fn_env : env) (env : env) (name : string) (incoming : type_)
  : type_
  =
  let entry = env_find fn_env env name in
  let incoming = unify_types incoming entry.type_ in
  let type_ =
    match entry.expr with
    | Some (env, expr) ->
      entry.expr <- None;
      infer_type fn_env env expr incoming
    | None -> incoming
  in
  let entry = env_find fn_env env name in
  entry.type_ <- type_;
  if type_is_fully_known type_
  then type_
  else user_error "Could not infer type of '%s' due to lack of information" name
;;

let type_of input =
  let ps = { input; input_len = String.length input; index = 0 } in
  let expr = parse_program ps in
  let fn_env = String_map.empty in
  let env = String_map.empty in
  let type_ = infer_type fn_env env expr Unknown in
  let output = { indent = 0; at_start_of_line = false; buffer = Buffer.create 1024 } in
  output_type output type_;
  Buffer.contents output.buffer
;;

type stack_instr =
  | Push_data of
      { size : int
      ; data : string
      }
  | Push_fn of { id : int }
  | Dup of { src : int }
  | Call

type stack_fn =
  { arity : int
  ; instrs : stack_instr list
  }

type stack_fns =
  { mutable fns : stack_fn Int_map.t
  ; mutable next_id : int
  }

let stack_fns_add (fns : stack_fns) (arity : int) (instrs : stack_instr list) : int =
  let id = fns.next_id in
  fns.next_id <- fns.next_id + 1;
  fns.fns <- Int_map.add id { arity; instrs } fns.fns;
  id
;;

type stack_env_entry_state =
  | Not_yet_compiled of stack_env * expr
  | On_stack of int
  | Fn_id of int

and stack_env = stack_env_entry_state ref String_map.t

let rec generate_stack_instrs
  (fns : stack_fns)
  (fn_env : stack_env)
  (env : stack_env)
  (expr : expr)
  : stack_instr list
  =
  match expr with
  | Identifier name ->
    let entry = String_map.find name env in
    (match !entry with
     | Not_yet_compiled (env, expr) ->
       (match expr with
        | Fn { arg_names; body } ->
          let id = generate_function fns fn_env arg_names body in
          entry := Fn_id id;
          [ Push_fn { id } ]
        | Identifier _ | Integer _ | Comment _ | Let _ | Rec _ | Match _ | Call _ ->
          generate_stack_instrs fns fn_env env expr)
     | On_stack src -> [ Dup { src } ]
     | Fn_id id -> [ Push_fn { id } ])
  | Integer { data; size } -> [ Push_data { size; data } ]
  | Comment { text = _; expr } -> generate_stack_instrs fns fn_env env expr
  | Let { binding; body } ->
    let entry = ref (Not_yet_compiled (env, binding.expr)) in
    let env = String_map.add binding.name entry env in
    generate_stack_instrs fns fn_env env body
  | Rec _ -> assert false
  | Match _ -> assert false
  | Fn { arg_names; body } ->
    let id = generate_function fns fn_env arg_names body in
    [ Push_fn { id } ]
  | Call { fn; args } ->
    ListLabels.concat_map (List.rev args) ~f:(fun arg ->
      generate_stack_instrs fns fn_env env arg)
    @ generate_stack_instrs fns fn_env env fn
    @ [ Call ]

and generate_function (fns : stack_fns) (fn_env : stack_env) arg_names body : int =
  (* TODO: Use fn_env and env separately instead of putting al the names in just env. *)
  let env, arity =
    ListLabels.fold_left
      arg_names
      ~init:(String_map.empty, 0)
      ~f:(fun (env, i) (arg_name, _) ->
        String_map.add arg_name (ref (On_stack i)) env, i + 1)
  in
  let instrs = generate_stack_instrs fns fn_env env body in
  stack_fns_add fns arity instrs
;;

let output_stack_instrs (output : output) (instrs : stack_instr list) : unit =
  iter_sep_by
    instrs
    ~sep:(fun () -> output_newline output)
    ~f:(fun instr ->
      match instr with
      | Push_data { size; data } ->
        output_string output "push ";
        output_string output data;
        output_string output ":";
        output_string output (Int.to_string size)
      | Push_fn { id } ->
        output_string output "fn ";
        output_string output (Int.to_string id)
      | Dup { src } ->
        output_string output "dup ";
        output_string output (Int.to_string src)
      | Call -> output_string output "call")
;;

let output_fns (output : output) (fns : stack_fns) : unit =
  ListLabels.iter
    (Int_map.to_seq fns.fns |> List.of_seq)
    ~f:(fun (id, { arity; instrs }) ->
      output_string output (Int.to_string id);
      output_string output " (arity ";
      output_string output (Int.to_string arity);
      output_string output ")";
      output_newline output;
      output_indent output;
      output_stack_instrs output instrs;
      output_dedent output;
      output_newline output;
      output_newline output)
;;

let stack_instrs input =
  let ps = { input; input_len = String.length input; index = 0 } in
  let expr = parse_program ps in
  let fn_env = String_map.empty in
  let env = String_map.empty in
  let (_ : type_) = infer_type fn_env env expr Unknown in
  let fns = { fns = Int_map.empty; next_id = 0 } in
  let instrs = generate_stack_instrs fns String_map.empty String_map.empty expr in
  let output = { indent = 0; at_start_of_line = false; buffer = Buffer.create 1024 } in
  output_fns output fns;
  output_stack_instrs output instrs;
  Buffer.contents output.buffer
;;
