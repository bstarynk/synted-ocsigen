(* -*- tuareg -*- *)
type
  expr_t =  { exp_num: int; exp_cont: expr_content_t }
and
  expr_content_t =
  | ExNil
  | ExInt of int
  | ExStr of string
  | ExVar of symbol_t
  | ExParen of expr_t
  | ExNot of expr_t
  | ExNeg of expr_t
  | ExAdd of expr_t * expr_t
  | ExSub of expr_t * expr_t
  | ExOr of expr_t * expr_t
  | ExMul of expr_t * expr_t
  | ExDiv of expr_t * expr_t
  | ExAnd of expr_t * expr_t
  | ExIf of expr_t * expr_t * expr_t
  | ExVect of expr_t array
  | ExApply of expr_t * expr_t array
  | ExLambda of symbol_t array * expr_t list
  | ExLet of binding_t array * expr_t list

and

  symbol_t =  { symb_num: int; symb_name: string }

and

  binding_t =  { bind_num: int;  bind_symb: symbol_t ; bind_expr: expr_t;
		 mutable bind_constructive: bool option }


let expr_counter = ref 0;;
let make_expr ec =
    incr expr_counter;
    { exp_num= !expr_counter; exp_cont= ec }
    ;;

let symbol_hash_table = Hashtbl.create ~random: true 241 ;;

let symbol name =
  try Hashtbl.find symbol_hash_table name
  with Not_found -> begin
  let cnt = Hashtbl.length symbol_hash_table
  in
  let newsymb = { symb_num= cnt+1; symb_name= name } in
  Hashtbl.add symbol_hash_table name newsymb ;
  newsymb
  end
  ;;



let binding_counter = ref 0;;
    
let make_binding symb expr =
  incr binding_counter;
  { bind_num= !binding_counter;
    bind_symb= symb;
    bind_expr= expr;
    bind_constructive= None
  };;
    
