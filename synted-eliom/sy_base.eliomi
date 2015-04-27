(* -*- tuareg -*- *)
type
  expr_t = private { exp_num: int; exp_cont: expr_content_t }
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

  symbol_t = private { symb_num: int; symb_name: string }

and

  binding_t = private { bind_num: int;  bind_symb: symbol_t ; bind_expr: expr_t; mutable bind_constructive: bool option }

;;

val  make_expr : expr_content_t -> expr_t;;

val  symbol : string -> symbol_t;;

val make_binding : symbol_t -> expr_t -> binding_t ;;
				  
