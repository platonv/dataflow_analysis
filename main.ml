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
and inN = var list
and outN = var list
and dataflowNode = DataFlowNode of node * inN * outN
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
  | NullStmt -> "NullStmt"

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

let xeqasumb = AssignStmt(
   (Var("x")),
   (ArithExp(Add(Id(Var("a")), Id(Var("b")))))
)
;;

let yeqamulb = AssignStmt(
   (Var("y")),
   (ArithExp(Mul(Id(Var("a")), Id(Var("b")))))
)
;;


let xeqaminb = AssignStmt(
   (Var("x")),
   (ArithExp(Sub(Id(Var("a")), Id(Var("b")))))
)
;;

let aeqasum1 = AssignStmt(
   (Var("a")),
   (ArithExp(Mul(Id(Var("a")), Number(Const(2))))))
;;
let while1 = WhileStmt(
   (ArithExp(Sub((Id(Var("y"))), (Id(Var("a")))))),
   (CompStmt (aeqasum1, xeqaminb))
)
;;

let p2 = Program (CompStmt (
  xeqasumb, CompStmt (
    yeqamulb, while1
    )
  ))



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
    | Node(stmt, pred, succ) :: tail -> "*************************\n" ^ (string_of_pred pred) ^ (string_of_stmt stmt) ^ (string_of_succ succ) ^ "*************************" ^ (string_of_cfg tail)
  ;;


let rec string_of_var_list (use_vars: var list) =
  match use_vars with
    | [] -> ""
    | v :: tail -> "~~~~" ^ (string_of_var v) ^ "\n" ^ string_of_var_list tail
  ;;

let in_of_node = function
  | DataFlowNode(_, i, _) -> i
  ;;

let out_of_node = function
  | DataFlowNode(_, _, out) -> out
  ;;

let node_of_dfNode = function
  | DataFlowNode(n, _, _) -> n
  ;;

let stmt_of = function
  | Node(s, _, _) -> s
  ;;

let rec string_of_dataflow_nodes (nodes: dataflowNode list) =
  let s = List.map (fun n ->
    (string_of_stmt (stmt_of (node_of_dfNode n))) ^ "\nIN:\n" ^ string_of_var_list (in_of_node n) ^ "\nOUT:\n" ^ string_of_var_list (out_of_node n)
  ) nodes in
  String.concat "\n" s
  ;;

let succ_of = function
  | Node(_, _, s) -> s
  ;;

(* let rec containsNull (stmts: stmt list) = 
  match stms with
    | [] -> false
    | NullStmt :: t -> true
    | _ :: t -> containsNull NullStmt
  ;; *)

let diff l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1
;;


let pred_of = function
  | Node(_, p, _) -> p
  ;;

exception Error;;
exception ErrorNodeOfDfNode;;

let rec dataflowNode_of_node (n: node) (dfNodes: dataflowNode list)=
  match dfNodes with
    | [] -> raise ErrorNodeOfDfNode

    | h :: tail -> match h with
                    | DataFlowNode(nn, _, _) -> if n = nn then
                                                h
                                              else
                                              dataflowNode_of_node n tail
  ;;

let rec node_of_statement (s: stmt) (nodes: nodeList) =
  match nodes with
  | [] -> raise Error
  | n :: tail -> match n with
                  | Node(st, _, _) -> if s == st then
                                      n
                                      else
                                      node_of_statement s tail
  ;;

let rec cfg_from_ast (currStmt: stmt) (postStmt: stmt) =
  match currStmt with
  | AssignStmt(_, _) -> [Node(currStmt, [], [postStmt])]
  | CompStmt(s1, s2) -> let s2Nodes = cfg_from_ast s2 postStmt in
                        let s1Nodes = (cfg_from_ast s1 (stmt_of (List.hd s2Nodes))) in
                            s1Nodes @ s2Nodes
  | IfStmt(_, stmt1, stmt2) -> let stmtCfg1 = cfg_from_ast stmt1 currStmt in
                              let stmtCfg2 = cfg_from_ast stmt2 currStmt in
                              let ifNode = Node(currStmt, [], [stmt_of (List.hd stmtCfg1); stmt_of (List.hd stmtCfg2)]) in
                              [ifNode] @ stmtCfg1 @ stmtCfg2
  | WhileStmt(_, stmt) -> let stmtCfg = cfg_from_ast stmt currStmt in
                          let whileNode = Node(currStmt, [], [stmt_of (List.hd stmtCfg); postStmt]) in
                              [whileNode] @ stmtCfg
  | PrintStmt(_) -> [Node(currStmt, [], [postStmt])]
  | _ -> []
  ;;

let rec statements_contain (s: stmt) (l: stmt list) =
  List.mem s l
  ;;


let rec find_predecessors (s: stmt) (nodes: nodeList) =
  match nodes with
  | [] -> []
  | Node(p, _, succ) :: t -> if List.mem s succ then
                             [p] @ (find_predecessors s t)
                             else
                             find_predecessors s t
  ;;

let rec proc (nodes: nodeList) =
  List.map (function | Node(st, _, s) -> Node(st, (find_predecessors st nodes), s)) nodes
  ;;

let rec hasIdArithExp = function
  | Add (exp1, exp2) -> hasId(exp1) @ hasId(exp2)
  | Sub (exp1, exp2) -> hasId(exp1) @ hasId(exp2)
  | Mul (exp1, exp2) -> hasId(exp1) @ hasId(exp2)
  | Div (exp1, exp2) -> hasId(exp1) @ hasId(exp2)

and hasId = function
  | Number (_) -> []
  | Id (v) -> [v]
  | ArithExp (exp) -> hasIdArithExp(exp)
;;



let kill (currentNode: node) (defList: var list) =
  let stmt = stmt_of currentNode in
  match stmt with
    | AssignStmt (var, exp) -> defList @ [var]
    | _ -> defList ;;

let gen (currentNode: node)  =
  let stmt = stmt_of currentNode in
  match stmt with
    | AssignStmt (_, exp) ->  hasId exp
    | IfStmt (exp, _, _) -> hasId exp
    | PrintStmt (exp) -> hasId exp
    | WhileStmt (exp, stmt) -> hasId exp;;



let print_kill (nodes: nodeList) =
  let s = List.map (function | Node(st, p, s) -> (string_of_stmt st)   ^ "KILL:" ^ (string_of_var_list (kill (Node(st,p,s)) []))) nodes in
  String.concat "\n" s
;;

let print_gen (nodes: nodeList) =
  let s = List.map (function | Node(st, p, s) -> (string_of_stmt st)   ^ "GEN:" ^ (string_of_var_list (gen (Node(st,p,s))))) nodes in
  String.concat "\n"  s
;;


let cfg (p: prg) =
  match p with
  | Program(stmt) -> cfg_from_ast stmt NullStmt
  ;;

let rev list =
  let rec aux acc = function
    | [] -> acc
    | h::t -> aux (h::acc) t in
  aux [] list
  ;;


let find_start_node (nodes: nodeList) =
  List.hd (List.filter (fun n -> List.length (pred_of n) == 0) nodes)


let rec all_succ_of_node (n: node) (nodes: nodeList) =
  let succ = succ_of n in
  let nodes_succ = List.map (fun suc -> node_of_statement suc nodes) succ in
  nodes_succ @ (List.flatten (List.map (fun n -> (all_succ_of_node n nodes)) nodes_succ))
;;

 let rec compute_in (node: node) (outs: var list) =
  let killN = kill node [] in
  let genN = gen node in
  let res = genN @ (diff outs killN) in
  List.sort_uniq (fun x y -> compare x y) res
;;


let compute_out (node: node) (nodes: nodeList) (dfNList: dataflowNode list) =
    let succ = (diff (succ_of node) [NullStmt])  in
    let nodes_succ = List.map (fun suc -> node_of_statement suc nodes) succ in
    let res = List.flatten (List.map (fun s -> (in_of_node (dataflowNode_of_node s dfNList))) nodes_succ) in
    List.sort_uniq (fun x y -> compare x y) res
  ;;

let rec replaceNode (newNode: dataflowNode) (allDfNodes: dataflowNode list) =
    match allDfNodes with
      | [] -> []
      | h :: tl -> if (node_of_dfNode h) == (node_of_dfNode newNode) then
                      newNode :: tl
                    else
                      h :: replaceNode newNode tl
;;


let rec whileLoop  (workList: dataflowNode list) (nodes: nodeList) (allDfNodes: dataflowNode list) =
  match workList with
    | [] -> allDfNodes
    | h :: t -> let outs = compute_out (node_of_dfNode h) nodes allDfNodes in
                let ins = compute_in (node_of_dfNode h) outs in
                let final_ins = List.sort_uniq (fun x y -> compare x y) (ins @ (in_of_node h)) in
                let final_outs = List.sort_uniq (fun x y -> compare x y) (outs @ (out_of_node h)) in
                let newNode = DataFlowNode((node_of_dfNode h), final_ins, final_outs) in
                if ins <> in_of_node h then
                  let prevs = pred_of (node_of_dfNode h) in
                  let nodes_prev = List.map (fun p -> node_of_statement p nodes) prevs in
                  let res = List.map (fun p -> (dataflowNode_of_node p allDfNodes)) nodes_prev in
                  whileLoop (t @ res) (nodes) (replaceNode newNode allDfNodes)
                else
                  whileLoop t nodes (replaceNode newNode allDfNodes)
                ;;

let backwardDataflow (nodes: nodeList) =
  let dfNodes = List.map (fun n -> DataFlowNode(n, [], [])) nodes in
  let initial = List.filter (fun n -> statements_contain NullStmt (succ_of n)) nodes in
  let initialDf = List.map (fun n -> DataFlowNode(n, (compute_in n []), [])) initial in
  let df = List.flatten (List.map (fun n-> replaceNode n dfNodes) initialDf) in
  let workList = (diff df initialDf) in
    whileLoop workList nodes df
  ;;






let final_cfg = (proc (cfg p));;

print_string "Program: \n" ;;
print_string "----------------------\n" ;;
print_string (string_of_program p) ;;
print_string "----------------------\n" ;;

print_string "CFG: \n" ;;
print_string "----------------------\n" ;;
print_string (string_of_cfg final_cfg) ;;
print_string "----------------------\n" ;;

(* let cfg = proc cfg p;;
 *)
print_string "kill: \n";;
print_string "----------------------\n" ;;
print_string (print_kill final_cfg) ;;
print_string "----------------------\n" ;;


print_string "GEN: \n";;
print_string "----------------------\n" ;;
print_string (print_gen final_cfg) ;;
print_string "----------------------\n" ;;

print_string "DF: \n";;
print_string "----------------------\n" ;;
print_string (string_of_dataflow_nodes (backwardDataflow final_cfg)) ;;
print_string "----------------------\n" ;;