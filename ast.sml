structure AST =
struct

type id = string
datatype binop = Plus | Minus | Times
datatype unop = Not | Neg
datatype logicop =   And | Or | Xor | Implies
datatype relop =     Equals | LessThan | GreaterThan
datatype decl = ValDecl of id * exp
and prg = Program of exp*prg
         | LastExp of exp
and exp = NumExp of int
    	| ConstExp of string
    	| VarExp of id
	| BinExp of binop * exp * exp
	| LetExp of decl * exp
	| UnExp of unop * exp	
	| LogicExp of logicop * exp * exp
	| RelExp of  relop*exp*exp 
	| IfExp of exp * exp * exp	
	| Fn of id*typ*typ*exp
	| Fun of id*id*typ*typ*exp
	| AppExp of exp*exp	   
and typ = INT
        | STRING
        | BOOL
        | ARROW of typ*typ 
        | TypeEnv of typeenvironment
withtype typeenvironment = (id * typ) list  
fun typeEnvAdd (var:id, v:typ, env:typeenvironment) =
    (var,v)::env

fun typeEnvLookup (var:id, env:typeenvironment) =
    case List.find(fn (x, _) => x = var) env of
				       SOME (x, v)   => v
				    |   NONE => raise Fail ("Unbound variable or constructor: " ^ var)
datatype value = IntVal of int
	       | BoolVal of bool	
	       | FunVal of id*exp*environment	
	       | Env of environment		
withtype environment = (id * value) list
fun envAdd (var:id, v:value, env:environment) =
    (var,v)::env

fun envLookup (var:id, env:environment) =
    case List.find(fn (x, _) => x = var) env of
				       SOME (x, v)   => v
				    |   NONE => raise Fail ("Unbound variable or constructor: " ^ var)
fun type_to_string(t: typ): string =
    case t of  INT => "int"
        | STRING => "string"
        | BOOL => "bool"
        | ARROW(t1, t2) => type_to_string(t1) ^ "->" ^ type_to_string(t2)   
fun exp_to_string(e: exp): string =
    case e of
	NumExp i            => Int.toString(i)
      | ConstExp s          => s
      | VarExp x            => x				  
      | BinExp (b, e1, e2)  => 
              (case b of Plus => exp_to_string(e1) ^ " PLUS " ^ exp_to_string(e2)
                       | Minus => exp_to_string(e1) ^ " MINUS " ^ exp_to_string(e2)
                       | Times => exp_to_string(e1) ^ " TIMES " ^ exp_to_string(e2)
              )
      | UnExp(u, e)         => 
              (case u of Not => " NOT " ^ exp_to_string(e)
                       | Negate => " NEGATE " ^ exp_to_string(e)
              )
      | LogicExp(l, e1, e2) => 
              (case l of And => exp_to_string(e1) ^ " AND " ^ exp_to_string(e2)
                       | Or => exp_to_string(e1) ^ " OR " ^ exp_to_string(e2)
                       | Xor => exp_to_string(e1) ^ " XOR " ^ exp_to_string(e2)
                       | Implies => exp_to_string(e1) ^ " IMPLIES " ^ exp_to_string(e2)
              )
      | RelExp(r, e1, e2)   => 
              (case r of Equals => exp_to_string(e1) ^ " EQUALS " ^ exp_to_string(e2)
                       | LessThan => exp_to_string(e1) ^ " LESSTHAN " ^ exp_to_string(e2)
                       | GreaterThan => exp_to_string(e1) ^ " GREATERTHAN " ^ exp_to_string(e2)
              )
      | Fn (farg, ty1, ty2, e1) => "fn " ^ " (" ^ farg ^ ": " ^ type_to_string(ty1) ^ "): " ^ type_to_string(ty2) ^"=>" ^ exp_to_string(e1)
      | Fun (f, farg, ty1, ty2, e1) => "fun " ^ f ^ " (" ^ farg ^ ": " ^ type_to_string(ty1) ^ "): " ^ type_to_string(ty2) ^"=>" ^ exp_to_string(e1)
      | AppExp (e1, e2) => "(" ^ exp_to_string(e1) ^ " " ^ exp_to_string(e2) ^ ")"
      | IfExp (e1, e2, e3)  => "if " ^ exp_to_string(e1) ^ " then " ^ exp_to_string(e2) ^ " else " ^ exp_to_string(e3)
      | LetExp(ValDecl(x, e1), e2)  => "let\n\t" ^ exp_to_string(e1) ^ "\nin\n\t" ^ exp_to_string(e2) ^ "\nend"  	 
end


