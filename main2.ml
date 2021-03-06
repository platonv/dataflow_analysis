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
  | IfStmt(exp, stmt1, stmt2) -> "if(" ^ (string_of_exp exp) ^ ")\n" ^ (string_of_stmt stmt1) ^  "else\n" ^ (string_of_stmt stmt2) ^ "end\n"
  | WhileStmt(exp, s) -> "while(" ^ (string_of_exp exp) ^ ")\n" ^ (string_of_stmt s) ^ "end\n"
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


let if1 = WhileStmt(
   (ArithExp(Sub((Id(Var("x"))), (Id(Var("y")))))),
   (*  (zeqy), *)
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
  | Node(_, _, [NullStmt]) -> []
  | Node(_, _, s) -> s
  ;;

let prev_of = function
  | Node(_, p, _) -> p
  ;;

exception ErrorNodeOfStmt;;
exception ErrorNodeOfDfNode;;

let rec node_of_statement (s: stmt) (nodes: nodeList) =
  match nodes with
  | [] -> raise ErrorNodeOfStmt
  | n :: tail -> match n with
                  | Node(st, _, _) -> if s == st then
                                      n
                                      else
                                      node_of_statement s tail
  ;;

let rec dataflowNode_of_node (n: node) (dfNodes: dataflowNode list)=
  match dfNodes with
    | [] -> None
    | h :: tail -> match h with
                    | DataFlowNode(nn, _, _) -> if n = nn then
                                                Some(h)
                                              else
                                              dataflowNode_of_node n tail
  ;;

let rec cfg_from_ast (currStmt: stmt) (postStmt: stmt) =
  match currStmt with
  | AssignStmt(_, _) -> [Node(currStmt, [], [postStmt])]
  | CompStmt(s1, s2) -> let s2Nodes = cfg_from_ast s2 postStmt in
                        let s1Nodes = (cfg_from_ast s1 (stmt_of (List.hd s2Nodes))) in
                            s1Nodes @ s2Nodes
  | IfStmt(_, stmt1, stmt2) -> let ifNode = Node(currStmt, [], [stmt1; stmt2]) in
                              [ifNode] @ (cfg_from_ast (stmt1) postStmt) @ (cfg_from_ast (stmt2) postStmt)
  | WhileStmt(_, stmt) -> let whileNode = Node(currStmt, [], [stmt; postStmt]) in
                              [whileNode] @ (cfg_from_ast stmt currStmt)
  | PrintStmt(_) -> [Node(currStmt, [], [postStmt])]
  | _ -> []
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



let gen (currentNode: node) (defList: var list) =
  let stmt = stmt_of currentNode in
  match stmt with
    | AssignStmt (var, exp) -> defList @ [var]
    | _ -> defList
;;

let use (currentNode: node)  =
  let stmt = stmt_of currentNode in
  match stmt with
    | AssignStmt (_, exp) ->  hasId exp
    | IfStmt (exp, _, _) -> hasId exp
    | WhileStmt (exp, stmt) -> hasId exp
    | PrintStmt (exp) -> hasId exp
    | _ -> []
;;

let print_def (nodes: nodeList) =
  let s = List.map (function | Node(st, p, s) -> (string_of_stmt st)   ^ "GEN:\n" ^ (string_of_var_list (gen (Node(st,p,s)) []))) nodes in
  String.concat "\n" s
;;

let print_use (nodes: nodeList) =
  let s = List.map (function | Node(st, p, s) -> (string_of_stmt st)   ^ "USE:\n" ^ (string_of_var_list (use (Node(st,p,s))))) nodes in
  String.concat "\n"  s
;;

let cfg = match p with
  | Program(stmt) -> cfg_from_ast stmt NullStmt
  ;;

 let rev list =
  let rec aux acc = function
    | [] -> acc
    | h::t -> aux (h::acc) t in
  aux [] list
  ;;

let diff l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1
;;

let compute_in (node: node) (outList: outN) =
  let defN = gen node [] in
  let useN = use node in
  let res = useN @ (diff outList defN) in
  List.sort_uniq (fun x y -> compare x y) res
;;


let rec dfNode_list (nodes: node list) (allNodes: node list) (dfNList: dataflowNode list) (result: dataflowNode list)= 
  match nodes with
    | [] -> result
    | h :: tl -> match dataflowNode_of_node h dfNList with
                  | Some(dfN) -> dfNode_list tl allNodes dfNList result@[dfN]
                  | None -> let outVars = compute_out h allNodes dfNList in
                            let dfNode = DataFlowNode (h, (compute_in h outVars), outVars) in
                            dfNode_list tl allNodes (dfNList) (result @ [dfNode])(* dfNList *)
      

and compute_out (node: node) (nodes: nodeList) (dfNList: dataflowNode list) =
    let succ = succ_of node in
    let nodes_succ = List.map (fun suc -> node_of_statement suc nodes) succ in
    let dfNodes_succ = dfNode_list nodes_succ nodes dfNList [] in
    let res = List.flatten (List.map (fun succ -> (in_of_node succ)) dfNodes_succ) in
    List.sort_uniq (fun x y -> compare x y) res
  ;;



let rec compute_rec_dataFlow (prevNodes: nodeList) (allNodes: nodeList) (dfNList: dataflowNode list) =
  (* match dfNList with
    | [] -> livePoints
    | n :: tl ->  let prev_of_node = prev_of (node_of_dfNode n) in
                  match prev_of_node with 
                    | [] -> compute_rec_dataFlow (allNodes) tl ([n]@fullDf) [n]@livePoints
                    | h :: tail -> 
                                  let currNode = (node_of_statement h allNodes) in 
                                  let outVars = compute_out currNode allNodes fullDf in
                                  let newNode = DataFlowNode (currNode, (compute_in currNode outVars), outVars) in
                                  compute_rec_dataFlow (allNodes) ([newNode] @ tl) ([n]@fullDf) ([n] @ dfNList) (* @ (compute_rec_dataFlow (allNodes) (tl) ([n]@fullDf) (dfNList)) *)
;; *)
    match prevNodes with
    | [] -> dfNList
    | n :: tl ->  let outVars = compute_out n allNodes dfNList in
                  let dfNode = DataFlowNode (n, (compute_in n outVars), outVars) in 
                  let prev_stmts = List.flatten (List.map (fun n-> prev_of n) tl) in
                  let prev_nodes = List.map (fun n-> node_of_statement n allNodes) prev_stmts in
                    compute_rec_dataFlow (tl @ prev_nodes) (allNodes) (dfNList@ [dfNode] )
(*                     [dfNode]
 *)                  ;;



let compute_initial_dataFlow (nodes: nodeList) =
  let initial = List.filter (fun n -> ((List.length (succ_of n)) == 0)) nodes in
  let initialDf = List.map (fun n -> DataFlowNode(n, (compute_in n []), [])) initial in
  let prev_stmts = List.flatten (List.map (fun n-> prev_of n) initial) in
  let prev_nodes = List.map (fun n-> node_of_statement n nodes) prev_stmts in
  (* compute_rec_dataFlow prev_nodes (nodes) (initialDf) *)
  initialDf
;;

print_string "Program: \n" ;;
print_string "----------------------\n" ;;
print_string (string_of_program p) ;;
print_string "----------------------\n" ;;

print_string "CFG: \n" ;;
print_string "----------------------\n" ;;
print_string (string_of_cfg (proc cfg)) ;;
print_string "----------------------\n" ;;

let cfg = proc cfg;;

print_string "GEN: \n";;
print_string "----------------------\n" ;;
print_string (print_def (proc cfg)) ;;
print_string "----------------------\n" ;;


print_string "USE: \n";;
print_string "----------------------\n" ;;
print_string (print_use (proc cfg)) ;;
print_string "----------------------\n" ;;

print_string "DF: \n";;
print_string "----------------------\n" ;;
print_string (string_of_dataflow_nodes (compute_initial_dataFlow cfg)) ;;
print_string "----------------------\n" ;;
