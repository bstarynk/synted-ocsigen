(* -*- tuareg -*- *)
(** file sy_base **)
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

      

module Symbol = struct
    type t = symbol_t;;
    let compare sy1 sy2 = if sy1==sy2 then 0
      else String.compare sy1.symb_name sy2.symb_name;;
    let equal sy1 sy2 = sy1 == sy2;;
    let hash sy = Hashtbl.hash sy.symb_name
end;;

module SymbolSet = Set.Make(Symbol);;
module SymbolHash = Hashtbl.Make(Symbol);;

let symbol_hash_table = Hashtbl.create ~random: true 241 ;;
let symbol_set = ref SymbolSet.empty;;

let symbol name =
  try Hashtbl.find symbol_hash_table name
  with Not_found -> begin
  let cnt = Hashtbl.length symbol_hash_table
  in
  let newsymb = { symb_num= cnt+1; symb_name= name } in
  Hashtbl.add symbol_hash_table name newsymb ;
  symbol_set := SymbolSet.add newsymb  !symbol_set;
  newsymb
  end
  ;;

let split_symbol_of_name name =
  let (before,present,after)
    = SymbolSet.split { symb_num=0; symb_name= name } !symbol_set
  in
  (before,
   (if present then Some (Hashtbl.find symbol_hash_table name) else None),
   after);;

let binding_counter = ref 0;;
    
let make_binding symb expr =
  incr binding_counter;
  { bind_num= !binding_counter;
    bind_symb= symb;
    bind_expr= expr;
    bind_constructive= None
  };;
    

let rec copy_expr ex =
  let exc = ex.exp_cont in
  match exc with
  | ExNil 
  | ExInt  _
  | ExStr  _
  | ExVar  _ -> make_expr exc
  | ExParen  e -> make_expr (ExParen (copy_expr e))
  | ExNot e -> make_expr (ExNot (copy_expr e))
  | ExNeg e -> make_expr (ExNeg (copy_expr e))
  | ExAdd (el,er) ->  make_expr (ExAdd ((copy_expr el), (copy_expr er)))
  | ExSub (el,er) ->  make_expr (ExSub ((copy_expr el), (copy_expr er)))
  | ExOr (el,er) ->   make_expr (ExOr ((copy_expr el), (copy_expr er)))
  | ExMul (el,er) ->  make_expr (ExMul ((copy_expr el), (copy_expr er)))
  | ExDiv (el,er) ->  make_expr (ExDiv ((copy_expr el), (copy_expr er)))
  | ExAnd (el,er) ->  make_expr (ExAnd ((copy_expr el), (copy_expr er)))
  | ExIf (econd,ethen,eelse) ->
     make_expr (ExIf ((copy_expr econd),
		      (copy_expr ethen),
		      (copy_expr eelse)))
  | ExVect vargs -> make_expr (ExVect (Array.map copy_expr vargs))
  | ExApply (efun,vargs) -> make_expr (ExApply ((copy_expr efun), (Array.map copy_expr vargs)))
  | ExLambda (symbarr, exlist) ->
     make_expr (ExLambda (symbarr, (List.map copy_expr exlist)))
  | ExLet (bindarr, exlist) ->
     make_expr (ExLet ((Array.map copy_binding bindarr),
		       (List.map copy_expr exlist)))
  and
    copy_binding {bind_symb= sy; bind_expr= exp}
    = make_binding sy (copy_expr exp)
;;
