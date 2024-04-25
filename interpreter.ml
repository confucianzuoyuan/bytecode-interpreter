let output_string s oc = Printf.fprintf oc "%s\n" s

type environment = (string * stackValue) list

and stackValue = 
  | Bool of bool
  | Integer of int
  | Error
  | String of string
  | Name of string
  | Unit
  | FunDef of string * string * command list * environment
  | InOutFunDef of string * string * command list

and command =
  | Add
  | Sub
  | Mul
  | Div
  | Neg
  | Rem
  | Swap
  | Cat
  | And
  | Or
  | Not
  | Equal
  | LessThan
  | Bind
  | If
  | Push of stackValue
  | Pop
  | Quit
  | Let
  | End
  | ToString
  | Println
  | Fun of string * string
  | InOutFun of string * string
  | FunEnd
  | Return
  | Call

let is_integer s = try let _ = int_of_string s in true with _ -> false

(* 检查字符是否是字母或下划线 *)
let is_alpha_or_underscore c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'

(* 检查字符是否是数字 *)
let is_digit c = c >= '0' && c <= '9'

(* 检查字符是否是有效的标识符字符：字母、数字或下划线 *)
let is_valid_identifier_char c =
  is_alpha_or_underscore c || is_digit c

(* 检查字符串是否是有效的标识符 *)
let is_valid_identifier s =
  if s = "" then false (* 空字符串不是有效的标识符 *)
  else
    let first_char = String.get s 0 in
    if not (is_alpha_or_underscore first_char) then false
    else
      let rec check_chars i =
        if i = String.length s then true (* 所有字符都有效 *)
        else
          let c = String.get s i in
          if is_valid_identifier_char c then check_chars (i + 1)
          else false
      in
      check_chars 1 (* 从第二个字符开始检查 *)

let string_to_stackvalue s = match s with
  | ":true:" -> Bool true
  | ":false:" -> Bool false
  | ":unit:" -> Unit
  | ":error:" -> Error
  | _s when String.get _s 0 == '"' -> String (String.sub _s 1 (String.length _s - 2))
  | _s when is_integer _s -> Integer (int_of_string _s)
  | name when is_valid_identifier name -> Name name
  | _ -> Error

let stackvalue_to_string = function
  | Bool true -> ":true:"
  | Bool false -> ":false:"
  | Unit -> ":unit:"
  | String s -> s
  | Integer i -> string_of_int i
  | Name name -> name
  | Error -> ":error:"
  | FunDef (name, _, _, _) -> "FunDef " ^ name
  | InOutFunDef (name, _, _) -> "InOutFunDef " ^ name 

let command_to_string = function
  | Add -> "add"
  | Sub -> "sub"
  | Mul -> "mul"
  | Div -> "div"
  | Neg -> "neg"
  | Rem -> "rem"
  | Swap -> "swap"
  | Cat -> "cat"
  | And -> "and"
  | Or -> "or"
  | Not -> "not"
  | Equal -> "equal"
  | LessThan -> "lessThan"
  | Bind -> "bind"
  | If -> "if"
  | Push stackValue -> "push " ^ (stackvalue_to_string stackValue)
  | Pop -> "pop"
  | Quit -> "quit"
  | Let -> "let"
  | End -> "end"
  | ToString -> "toString"
  | Println -> "println"
  | Fun (name, arg) -> "fun " ^ name ^ " " ^ arg
  | InOutFun (name, arg) -> "inOutFun " ^ name ^ " " ^ arg
  | FunEnd -> "funEnd"
  | Return -> "return"
  | Call -> "call"

let rec print_command_list = function
  | [] -> ()
  | hd :: tl -> let _ = print_string ((command_to_string hd) ^ "\n") in print_command_list tl

let rec print_stackvalue_list = function
  | [] -> ()
  | hd :: tl -> let _ = print_string ("res: " ^ (stackvalue_to_string hd) ^ "\n") in print_stackvalue_list tl

let string_to_command = function
  | "add" -> Add
  | "sub" -> Sub
  | "mul" -> Mul
  | "div" -> Div
  | "neg" -> Neg
  | "rem" -> Rem
  | "swap" -> Swap
  | "cat" -> Cat
  | "and" -> And
  | "or" -> Or
  | "not" -> Not
  | "equal" -> Equal
  | "lessThan" -> LessThan
  | "bind" -> Bind
  | "if" -> If
  | "pop" -> Pop
  | "quit" -> Quit
  | "let" -> Let
  | "end" -> End
  | "toString" -> ToString
  | "println" -> Println
  | "funEnd" -> FunEnd
  | "return" -> Return
  | "call" -> Call
  | cmd ->
    let cmd_list =  String.split_on_char ' ' cmd in
    match cmd_list with
    | ["fun"; funname; arg] -> Fun (funname, arg)
    | ["inOutFun"; funname; arg] -> InOutFun (funname, arg)
    | "push" :: tl -> Push (string_to_stackvalue (String.sub cmd 5 (String.length cmd - 5)))
    | _ -> failwith ("no command: " ^ cmd)

let rec get env name =
   match env with
   | [] -> Error
   | hd :: tl -> if String.compare (fst hd) name == 0 then snd hd else get tl name

let rec print_env env =
   match env with
   | [] -> ()
   | hd :: tl -> let _ = print_string (fst hd) in let _ = print_string " ==> " in
      let _ = print_string (stackvalue_to_string (snd hd)) in
      let _ = print_string "\n" in
      print_env tl

let add a b env =
   match a, b with
   | Integer i, Integer j -> [Integer (i + j)]
   | Name name, Integer j ->
      (match get env name with
       | Error -> [Error; a; b]
       | Integer i -> [Integer (i + j)]
       | _ -> [Error; a; b])
   | Integer i, Name name ->
      (match get env name with
       | Error -> [Error; a; b]
       | Integer j -> [Integer (i + j)]
       | _ -> [Error; a; b])
   | Name n1, Name n2 ->
      (match get env n1, get env n2 with
       | Error, _ -> [Error; a; b]
       | _, Error -> [Error; a; b]
       | Integer i, Integer j -> [Integer (i + j)]
       | _ -> [Error; a; b])
   | _ -> [Error; a; b]

let cat a b env =
   match a, b with
   | String i, String j -> [String (j ^ i)]
   | Name name, String j ->
      (match get env name with
       | Error -> [Error; a; b]
       | String i -> [String (j ^ i)]
       | _ -> [Error; a; b])
   | String i, Name name ->
      (match get env name with
       | Error -> [Error; a; b]
       | String j -> [String (j ^ i)]
       | _ -> [Error; a; b])
   | Name n1, Name n2 ->
      (match get env n1, get env n2 with
       | Error, _ -> [Error; a; b]
       | _, Error -> [Error; a; b]
       | String i, String j -> [String (j ^ i)]
       | _ -> [Error; a; b])
   | _ -> [Error; a; b]

let sub a b env =
   match a, b with
   | Integer i, Integer j -> [Integer (j - i)]
   | Name name, Integer j ->
      (match get env name with
       | Error -> [Error; a; b]
       | Integer i -> [Integer (j - i)]
       | _ -> [Error; a; b])
   | Integer i, Name name ->
      (match get env name with
       | Error -> [Error; a; b]
       | Integer j -> [Integer (j - i)]
       | _ -> [Error; a; b])
   | Name n1, Name n2 ->
      (match get env n1, get env n2 with
       | Error, _ -> [Error; a; b]
       | _, Error -> [Error; a; b]
       | Integer i, Integer j -> [Integer (j - i)]
       | _ -> [Error; a; b])
   | _ -> [Error; a; b]

let mul a b env =
   match a, b with
   | Integer i, Integer j -> [Integer (i * j)]
   | Name name, Integer j ->
      (match get env name with
       | Error -> [Error; a; b]
       | Integer i -> [Integer (i * j)]
       | _ -> [Error; a; b])
   | Integer i, Name name ->
      (match get env name with
       | Error -> [Error; a; b]
       | Integer j -> [Integer (i * j)]
       | _ -> [Error; a; b])
   | Name n1, Name n2 ->
      (match get env n1, get env n2 with
       | Error, _ -> [Error; a; b]
       | _, Error -> [Error; a; b]
       | Integer i, Integer j -> [Integer (i * j)]
       | _ -> [Error; a; b])
   | _ -> [Error; a; b]

let div a b env =
   match a, b with
   | Integer i, Integer j -> [Integer (j / i)]
   | _ -> [Error; a; b]

let rem a b env =
   match a, b with
   | Integer i, Integer j -> [Integer (j mod i)]
   | _ -> [Error; a; b]

let neg a env =
   match a with
   | Integer i -> [Integer (-i)]
   | Name name ->
      (match get env name with
       | Error -> [Error; a]
       | Integer i -> [Integer (-i)]
       | _ -> [Error; a])
   | _ -> [Error; a]

let op_and a b env =
   match a, b with
   | _, Bool false -> [Bool false]
   | Bool false, _ -> [Bool false]
   | Bool true, Bool true -> [Bool true]
   | Name name, Bool true ->
      (match get env name with
       | Error -> [Error; a; b]
       | Bool i -> [Bool i]
       | _ -> [Error; a; b])
   | Name n1, Name n2 ->
      (match get env n1, get env n2 with
       | Error, _ -> [Error; a; b]
       | _, Error -> [Error; a; b]
       | Bool i, Bool j -> [Bool (i && j)]
       | _ -> [Error; a; b])
   | _ -> [Error; a; b]

let op_or a b env =
   match a, b with
   | _, Bool true -> [Bool true]
   | Bool true, _ -> [Bool true]
   | Bool false, Bool false -> [Bool false]
   | Bool false, Name name ->
      (match get env name with
       | Error -> [Error; a; b]
       | Bool i -> [Bool i]
       | _ -> [Error; a; b])
   | Name name, Bool false ->
      (match get env name with
       | Error -> [Error; a; b]
       | Bool i -> [Bool i]
       | _ -> [Error; a; b])
   | Name n1, Name n2 ->
      (match get env n1, get env n2 with
       | Error, _ -> [Error; a; b]
       | _, Error -> [Error; a; b]
       | Bool i, Bool j -> [Bool (i || j)]
       | _ -> [Error; a; b])
   | _ -> [Error; a; b]

let op_not a env =
   match a with
   | Bool true -> [Bool false]
   | Bool false -> [Bool true]
   | _ -> [Error; a]

let op_equal a b env =
   match a, b with
   | Integer i, Integer j -> if i == j then [Bool true] else [Bool false]
   | Name name, Integer j ->
      (match get env name with
       | Error -> [Error; a; b]
       | Integer i -> if i == j then [Bool true] else [Bool false]
       | _ -> [Error; a; b])
   | Integer i, Name name ->
      (match get env name with
       | Error -> [Error; a; b]
       | Integer j -> if i == j then [Bool true] else [Bool false]
       | _ -> [Error; a; b])
   | Name n1, Name n2 ->
      (match get env n1, get env n2 with
       | Error, _ -> [Error; a; b]
       | _, Error -> [Error; a; b]
       | Integer i, Integer j -> if i == j then [Bool true] else [Bool false]
       | _ -> [Error; a; b])
   | _ -> [Error; a; b]

let less_than a b env =
   match a, b with
   | Integer i, Integer j -> if i > j then [Bool true] else [Bool false]
   | Name name, Integer j ->
      (match get env name with
       | Error -> [Error; a; b]
       | Integer i -> [Bool (i > j)]
       | _ -> [Error; a; b])
   | Integer i, Name name ->
      (match get env name with
       | Error -> [Error; a; b]
       | Integer j -> [Bool (i > j)]
       | _ -> [Error; a; b])
   | Name n1, Name n2 ->
      (match get env n1, get env n2 with
       | Error, _ -> [Error; a; b]
       | _, Error -> [Error; a; b]
       | Integer i, Integer j -> [Bool (i > j)]
       | _ -> [Error; a; b])
   | _ -> [Error; a; b]

let bind a b env =
   match a, b with
   | Integer i, Name name -> ([Unit], (name, a) :: env)
   | String s, Name name -> ([Unit], (name, a) :: env)
   | Unit, Name name -> ([Unit], (name, a) :: env)
   | Bool _, Name name -> ([Unit], (name, a) :: env)
   | Name n1, Name n2 ->
      (match get env n1 with
       | Error -> ([Error; a; b], env)
       | v -> ([Unit], (n2, v) :: env))
   | _ -> ([Error; a; b], env)

let cmd_if a b c env =
   match c with
   | Bool true -> [a]
   | Bool false -> [b]
   | Name name ->
      (match get env name with
      | Bool true -> [a]
      | Bool false -> [b]
      | _ -> [Error; a; b; c])
   | _ -> [Error; a; b; c]

let rec parse_fundef cmds acc =
   match Stream.next cmds with
   | FunEnd -> acc
   | cmd -> cmd :: parse_fundef cmds acc

let rec execute commands stack env oc = try
   match Stream.next commands with
   (* | [] -> stack *)
   | (Push stack_value) -> execute commands (stack_value :: stack) env oc
   | Pop ->
      (match stack with
       | [] -> execute commands [Error] env oc
       | shd :: stl -> execute commands stl env oc)
   | Add ->
      (match stack with
       | [] -> execute commands [Error] env oc
       | [v] -> execute commands [Error; v] env oc
       | a :: b :: stl -> execute commands ((add a b env) @ stl) env oc)
   | Sub ->
      (match stack with
       | [] -> execute commands [Error] env oc
       | [v] -> execute commands [Error; v] env oc
       | a :: b :: stl -> execute commands ((sub a b env) @ stl) env oc)
   | Mul ->
      (match stack with
       | [] -> execute commands [Error] env oc
       | [v] -> execute commands [Error; v] env oc
       | a :: b :: stl -> execute commands ((mul a b env) @ stl) env oc)
   | Div ->
      (match stack with
       | [] -> execute commands [Error] env oc
       | [v] -> execute commands [Error; v] env oc
       | a :: b :: stl -> execute commands ((div a b env) @ stl) env oc)
   | Rem ->
      (match stack with
       | [] -> execute commands [Error] env oc
       | [v] -> execute commands [Error; v] env oc
       | a :: b :: stl -> execute commands ((rem a b env) @ stl) env oc)
   | Neg ->
      (match stack with
       | [] -> execute commands [Error] env oc
       | shd :: stl -> execute commands ((neg shd env) @ stl) env oc)
   | Not ->
      (match stack with
       | [] -> execute commands [Error] env oc
       | shd :: stl -> execute commands ((op_not shd env) @ stl) env oc)
   | Swap ->
      (match stack with
       | [] -> execute commands [Error] env oc
       | [v] -> execute commands [Error; v] env oc
       | a :: b :: stl -> execute commands (b :: a :: stl) env oc)
   | And ->
      (match stack with
       | [] -> execute commands [Error] env oc
       | [v] -> execute commands [Error; v] env oc
       | a :: b :: stl -> execute commands ((op_and a b env) @ stl) env oc)
   | Or ->
      (match stack with
       | [] -> execute commands [Error] env oc
       | [v] -> execute commands [Error; v] env oc
       | a :: b :: stl -> execute commands ((op_or a b env) @ stl) env oc)
   | Equal ->
      (match stack with
       | [] -> execute commands [Error] env oc
       | [v] -> execute commands [Error; v] env oc
       | a :: b :: stl -> execute commands ((op_equal a b env) @ stl) env oc)
   | LessThan ->
      (match stack with
       | [] -> execute commands [Error] env oc
       | [v] -> execute commands [Error; v] env oc
       | a :: b :: stl -> execute commands ((less_than a b env) @ stl) env oc)
   | Cat ->
      (match stack with
       | [] -> execute commands [Error] env oc
       | [v] -> execute commands [Error; v] env oc
       | a :: b :: stl -> execute commands ((cat a b env) @ stl) env oc)
   | Println ->
      (match stack with
       | [] -> execute commands [Error] env oc
       | shd :: stl -> let _ = output_string (stackvalue_to_string shd) oc in execute commands stl env oc)
   | ToString ->
      (match stack with
       | [] -> execute commands [Error] env oc
       | shd :: stl -> execute commands ((String (stackvalue_to_string shd)) :: stl) env oc)
   | Bind ->
      (match stack with
       | [] -> execute commands [Error] env oc
       | [v] -> execute commands [Error; v] env oc
       | k :: v :: stl ->
         let (res, new_env) = bind k v env in
         execute commands (res @ stl) new_env oc)
   | If ->
      (match stack with
       | [] -> execute commands [Error] env oc
       | [v] -> execute commands [Error; v] env oc
       | [a; b] -> execute commands [Error; a; b] env oc
       | a :: b :: c :: stl ->
         execute commands ((cmd_if a b c env) @ stl) env oc)
   | Let ->
      let (res, _) = execute commands [] env oc in
      execute commands (res @ stack) env oc
   | End ->
      (match stack with
       | [] -> ([Error], env)
       | shd :: _ -> ([shd], env))
   | Fun (name, arg) ->
      let block = parse_fundef commands [] in
      let new_env = (name, FunDef (name, arg, block, env)) :: env in
      execute commands (Unit :: stack) new_env oc
   | InOutFun (name, arg) ->
      let block = parse_fundef commands [] in
      let new_env = (name, InOutFunDef (name, arg, block)) :: env in
      execute commands (Unit :: stack) new_env oc
   | Call ->
      (match stack with
       | [] -> execute commands [Error] env oc
       | [v] -> execute commands [Error; v] env oc
       | param :: (Name name) :: tl ->
         let (res, new_env) = execute_funcall name param env oc in
         (match res with
         | Error -> execute commands (Error :: stack) new_env oc
         | _ -> execute commands (res :: tl) new_env oc)
       | _ -> execute commands (Error :: stack) env oc)
   | Return ->
      (match stack with
       | [] -> (stack, env)
       | shd :: _ -> ([shd], env))
   | _ -> (stack, env)
   with _ -> (stack, env)

and execute_funcall funname param env oc =
   match get env funname with
   | FunDef (name, arg, body, closure) ->
      let arg_value = match param with
      | Name n -> get env n
      | _ -> param
      in
      let new_env = [(arg, arg_value)] @ closure @ env in
      let _ = print_env new_env in
      let (res, new_env) = execute (Stream.of_list body) [] new_env oc in
      (match res with
      | [v] -> (v, env)
      | _ -> (Error, env))
   | InOutFunDef (name, arg, body) ->
      (match param with
      | Name n ->
         let new_env = (arg, get env n) :: env in
         let (res, new_env) = execute (Stream.of_list body) [] new_env oc in
         (match res with
         | [v] -> (v, (n, get new_env arg) :: new_env)
         | _ -> (Error, env))
      | _ -> (Error, env))
   | _ -> (Error, env)

let interpreter (input, output) =
  let ic = open_in input
  in
  let oc = open_out output
  in
  let rec loop_read acc =
    try
      let l = String.trim(input_line ic) in loop_read (l::acc)
    with
    | End_of_file -> List.rev acc
  in
  let string_list = loop_read []
  in
  let com_list = List.map string_to_command string_list
  in
  let command_stream = Stream.of_list com_list in
  let _ = execute command_stream [] [] oc in
  ()

let () = interpreter ("input.txt", "output.txt")