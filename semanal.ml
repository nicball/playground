(*********************************
 * Debugging
 *)

let debug = false;;

let print_endline_debug s =
    if debug then print_endline s;;

(*********************************
 * Lexer
 *)

type token =
    | IdToken of string
    | KeywordToken of string
    | OpToken of char;;

let isalpha = function 
    | 'a' .. 'z' | 'A' .. 'Z' -> true
    | _ -> false;;

let isdigit = function
    | '0' .. '9' -> true
    | _ -> false;;

let isalnum c = isalpha c || isdigit c;;

exception StreamExn;;

let char_buf = ref (None : char option);;

let input_char_safe in_ch =
    try input_char in_ch
    with End_of_file -> '$'

let get_char () =
    match !char_buf with
    | None -> input_char_safe stdin
    | Some c -> char_buf := None; c;;

let rec peek_char () =
    match !char_buf with
    | None ->
        char_buf := Some (input_char_safe stdin);
        peek_char ()
    | Some c -> c;;

exception ParseExn of string;;

let token_buf = ref ([] : token list);;
let token_buf_size = 2;;

let get_word () =
    let rec loop s =
        if isalnum (peek_char ())
        then loop (s ^ String.make 1 (get_char ()))
        else s in
    let word = loop "" in
    match word with
    | "proc" | "s" | "integer" | "real" -> KeywordToken word
    | _ -> IdToken word;;

let token_to_string = function
    | IdToken name -> name
    | KeywordToken name -> name
    | OpToken op -> String.make 1 op;;

let print_token_buf () =
    if debug then begin
        print_string "token_buf=";
        List.iter (fun x -> x |> token_to_string |> print_string) !token_buf;
        print_char '\n'
    end;;

let rec do_get_token () =
    print_endline_debug "calling do_get_token";
    match peek_char () with
    | '$' | ';' | ':' | '^' -> OpToken (get_char ())
    | ' ' | '\t' | '\n' ->
        ignore (get_char ());
        do_get_token ()
    | _ ->
        if isalpha (peek_char ()) then get_word ()
        else raise (ParseExn "unexpected char");;

let fill_token_buf () =
    print_endline_debug "calling fill_token_buf";
    let rec loop n =
        if debug then Printf.printf "loop %i\n" n;
        if n = 0
        then []
        else
            let t = do_get_token () in
            t :: loop (n - 1) in
    let lst = loop (token_buf_size - List.length !token_buf) in
    token_buf := !token_buf @ lst;
    print_token_buf ();;

let rec get_token () =
    match !token_buf with
    | [] -> fill_token_buf (); get_token ()
    | t :: rest -> token_buf := rest; print_token_buf (); t;;

let rec (peek_token : int -> token) = function
    | n when n < token_buf_size ->
        print_endline_debug "calling peek_token";
        begin match List.nth_opt !token_buf n with
        | Some t -> t
        | None -> fill_token_buf (); peek_token n
        end
    | _ -> invalid_arg "peek_token";;


let expect t =
    if t = peek_token 0
    then ignore (get_token ())
    else raise (ParseExn "unexpected token");;

(*********************************
 * Symbol Table
 *)

type atype = RealType | IntType | PtrType of atype;;

type symtab = {
    mutable entries: symtab_entry list;
    mutable total_width: int;
}

and symtab_entry =
    | IdEntry of string * atype * int
    | ProcEntry of string * symtab;;

let modify_top f stack =
    let t = Stack.pop stack in
    Stack.push (f t) stack;;

let offsets = (Stack.create () : int Stack.t);;

let ststack = (Stack.create () : symtab Stack.t);;

let enter_proc () =
    Stack.push { entries = []; total_width = 0 } ststack;
    Stack.push 0 offsets;;

let add_entry entry =
    let top = Stack.top ststack in
    top.entries <- entry :: top.entries;;

let leave_proc name =
    let off = Stack.pop offsets in
    (Stack.top ststack).total_width <- off;
    let top = Stack.pop ststack in
    add_entry (ProcEntry (name, top));;

let rec atype_to_string = function
    | IntType -> "int"
    | RealType -> "real"
    | PtrType ty -> "^ " ^ atype_to_string ty;;

let rec print_symtab indent symtab =
    let print_indent indent =
        print_string (String.make (4 * indent) ' ') in
    let print_entry indent = function
        | IdEntry (name, ty, width) ->
            print_indent indent;
            Printf.printf "%s: %s width=%i\n"
                name (atype_to_string ty) width
        | ProcEntry (name, st) ->
            print_indent indent;
            Printf.printf "%s: proc = {\n" name;
            print_symtab (indent + 1) st;
            print_indent indent;
            print_string "}\n" in
    print_indent indent;
    Printf.printf "total_width=%i\n" symtab.total_width;
    print_indent indent;
    print_endline "entries:";
    List.iter (print_entry (indent + 1)) symtab.entries;;

(**************************
 * Parser
 *)

let rec parse_P () =
    print_endline_debug "calling parse P";
    enter_proc (); (* M *)
    parse_Ds ();
    expect (OpToken '$');
    (Stack.top ststack).total_width <- Stack.top offsets;
    ignore (Stack.pop offsets);
    Stack.pop ststack

and parse_Ds () =
    print_endline_debug "calling parse Ds";
    parse_D ();
    if peek_token 0 = OpToken ';'
    && peek_token 1 <> KeywordToken "s"
    then begin
        ignore (get_token ());
        parse_Ds ()
    end

and parse_D () =
    print_endline_debug "calling parse D";
    match peek_token 0 with
    | KeywordToken "proc" ->
        print_endline_debug "hit proc";
        ignore (get_token ());
        let IdToken pname = get_token () in
        expect (OpToken ';');
        enter_proc (); (* N *)
        parse_Ds ();
        expect (OpToken ';');
        expect (KeywordToken "s");
        leave_proc pname
    | IdToken id ->
        print_endline_debug "hit id";
        ignore (get_token ());
        expect (OpToken ':');
        let (ty, width) = parse_T () in
        add_entry (IdEntry (id, ty, width));
        modify_top (fun x -> x + width) offsets
    | _ -> raise (ParseExn "cannot parse D")

and parse_T () =
    print_endline_debug "calling parse T";
    match peek_token 0 with
    | KeywordToken "integer" ->
        ignore (get_token ());
        (IntType, 4)
    | KeywordToken "real" ->
        ignore (get_token ());
        (RealType, 8)
    | OpToken '^' ->
        ignore (get_token ());
        let (ty, _) = parse_T () in
        (PtrType ty, 4)
    | _ ->
        print_string (token_to_string (peek_token 0));
        raise (ParseExn "cannot parse T");;


(*********
 * Main
 *)

let rec main () =
    print_endline_debug "calling main";
    if debug then
        Printf.printf "ststack(%i) offsets(%i)\n"
            (Stack.length ststack) (Stack.length offsets);
    if peek_token 0 <> OpToken '$' then
        let symtab = parse_P () in
        print_symtab 0 symtab;
        print_endline "";
        main ();;

let () = main ();;
