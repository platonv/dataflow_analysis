open Types;;

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


