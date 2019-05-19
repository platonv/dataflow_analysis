(* TYPES *)
type const = Const of int
and var = Var of string
and
arithExp = Add of exp * exp
  | Sub of exp * exp
  | Mul of exp * exp
  | Div of exp * exp
and
exp = Number of const
  | Id of var
  | ArithExp of arithExp
and
stmt = CompStmt of stmt * stmt
  | AssignStmt of var * exp
  | PrintStmt of exp
  | IfStmt of exp * stmt * stmt
  | WhileStmt of exp * stmt
  | NullStmt
and prg = Program of stmt

and node = Node of stmt * pred * succ
and nodeList = node list
and pred = stmt list
and succ = stmt list
and cfg = CFG of nodeList
;;

let rec string_of_var = function
  | Var (s) -> s

and string_of_const = function
  | Const (c) -> string_of_int c

and string_of_arithExp = function
  | Add (exp1, exp2) -> (string_of_exp exp1)  ^ " + " ^ (string_of_exp exp2)
  | Sub (exp1, exp2) -> (string_of_exp exp1) ^ " - " ^ (string_of_exp exp2)
  | Mul (exp1, exp2) -> (string_of_exp exp1) ^ " * " ^ (string_of_exp exp2)
  | Div (exp1, exp2) -> (string_of_exp exp1) ^ " / " ^ (string_of_exp exp2)

and string_of_exp = function
  | Number (c) -> (string_of_const c) ;
  | Id (v) -> (string_of_var v) ;
  | ArithExp (exp) -> (string_of_arithExp exp);

and string_of_stmt = function
  | CompStmt(stmt1, stmt2) -> (string_of_stmt stmt1) ^ (string_of_stmt stmt2)
  | AssignStmt(var, exp) -> (string_of_var var) ^ " := " ^ (string_of_exp exp) ^ "\n" 
  | PrintStmt(exp) -> "print(" ^ (string_of_exp exp) ^ ")" ^ "\n" 
  | IfStmt(exp, stmt1, stmt2) -> "if(" ^ (string_of_exp exp) ^ ")\n" ^ (string_of_stmt stmt1) ^  "else\n" ^ (string_of_stmt stmt2) ^ "end \n"
  | WhileStmt(exp, s) -> "while(" ^ (string_of_exp exp) ^ ")\n" ^ (string_of_stmt s) ^ "\n" 
  | NullStmt -> ""

 and string_of_program = function
  |  Program stmt -> string_of_stmt stmt
  ;;

let yeq4 = AssignStmt(
   (Var("y")),
   (Number(Const(4)))
)
;;

let xeq2 = AssignStmt(
   (Var("x")),
   (Number(Const(2)))
)
;;

let zeqy = AssignStmt(
   (Var("z")),
   (Id(Var("y")))
)
;;

let xeqz = AssignStmt(
   (Var("x")),
   (Id(Var("z")))
)
;;

let zeqyy = AssignStmt(
   (Var("z")),
   (ArithExp(Mul(Id(Var("y")), Id(Var("y")))))
)
;;


let if1 = IfStmt(
   (ArithExp(Sub((Id(Var("x"))), (Id(Var("y")))))),
   (zeqy),
   (zeqyy)
)
;;

let p = Program(CompStmt(
  yeq4, CompStmt(
    xeq2, CompStmt(
      if1, xeqz
    )
  )
))
;;

let rec string_of_pred (cfg: stmt list) = 
  match cfg with
    | [] -> ""
    | stmt :: tail -> "\t<-" ^ (string_of_stmt stmt) ^ "\n" ^ (string_of_pred tail)
  ;;


let rec string_of_succ (cfg: stmt list) = 
  match cfg with
    | [] -> ""
    | stmt :: tail -> "\t->" ^ (string_of_stmt stmt) ^ "\n" ^ (string_of_succ tail)
  ;;


let rec string_of_cfg (cfg: nodeList) = 
  match cfg with
    | [] -> ""
    | Node(stmt, pred, succ) :: tail -> (string_of_pred pred) ^ (string_of_stmt stmt) ^ (string_of_succ succ) ^ (string_of_cfg tail)
  ;;

let stmt_of = function
  | Node(s, _, _) -> s
  ;;

exception Error;;

let rec node_of_statement (s: stmt) (nodes: nodeList) =
  match nodes with
  | [] -> raise Error
  | n :: tail -> match n with 
                  | Node(st, _, _) -> if s == st then
                                      n
                                      else
                                      node_of_statement s tail
  ;;

let rec cfg_from_ast (currStmt: stmt) (nodes: nodeList) (postStmt: stmt) =
  match currStmt with
  | AssignStmt(_, _) -> nodes @ [Node(currStmt, [], [postStmt])]
  | CompStmt(s1, s2) -> let s2Nodes = cfg_from_ast s2 nodes postStmt in
                        let s1Nodes = (cfg_from_ast s1 nodes (stmt_of (List.hd s2Nodes))) in
                            nodes @ s1Nodes @ s2Nodes
  | IfStmt(_, stmt1, stmt2) -> let ifNode = Node(currStmt, [], [stmt1; stmt2]) in
                              [ifNode] @ (cfg_from_ast (stmt1) nodes postStmt) @ (cfg_from_ast (stmt2) nodes postStmt)
  | _ -> nodes
  ;;

let rec statements_contain (s: stmt) (l: stmt list) = 
  match l with
  | [] -> false
  | h :: t -> if s = h then
              true
              else
              statements_contain s t
  ;;

let rec find_predecessors (s: stmt) (nodes: nodeList) = 
  match nodes with
  | [] -> []
  | Node(p, _, succ) :: t -> if statements_contain s succ then
                             [p] @ (find_predecessors s t)
                             else
                             find_predecessors s t
  ;;

let rec proc (nodes: nodeList) =
  List.map (function | Node(st, _, s) -> Node(st, (find_predecessors st nodes), s)) nodes
  ;;


let cfg = match p with
  | Program(stmt) -> cfg_from_ast stmt [] NullStmt
  ;;

print_string "Program: \n" ;;
print_string "----------------------\n" ;;
print_string (string_of_program p) ;;
print_string "----------------------\n" ;;

print_string "CFG: \n" ;;
print_string "----------------------\n" ;;
print_string (string_of_cfg (proc cfg)) ;;
print_string "----------------------\n" ;;
