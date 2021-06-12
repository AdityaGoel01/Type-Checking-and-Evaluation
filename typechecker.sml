structure TYPECHECKER  =
struct
open AST

fun checkTypePrg(p:prg, env:typeenvironment) =
    case p of 
        Program (e, p1) => 
                (case checkTypeExp(e, env) of 
                      TypeEnv env1 => checkTypePrg(p1, env1)
                      | _         => checkTypePrg(p1, env))
      | LastExp (e)         => checkTypeExp(e, env)      
and checkTypeExp(e:exp, env:typeenvironment): typ =
    case e of
	NumExp i            => INT
      | ConstExp s         => BOOL
      | VarExp x            => typeEnvLookup (x, env) 				  
      | BinExp (b, e1, e2)  => checkTypeBinExp(b, e1, e2, env)
      | UnExp(u, e)         => checkTypeUnExp(u, e, env)
      | LogicExp(l, e1, e2) => checkTypeLogicExp(l, e1, e2, env)
      | RelExp(r, e1, e2)   => checkTypeRelExp(r, e1, e2, env)
      | Fn (farg, ty1, ty2, e1) => 
            let 
                val typeExp = checkTypeExp(e1, typeEnvAdd(farg, ty1, env))
            in
                if(ty2=typeExp) then ARROW(ty1, typeExp) else raise Fail ("\n" ^ "Type Error: Mismatch in declared type and actual type" ^ "\n\t" ^ "\tdeclared type: " ^ type_to_string(ty2) ^ "\n\t" ^ "\tactual type: " ^ type_to_string(typeExp) ^ "\n\t" ^ "in epression" ^ "\n\t" ^ "\t" ^ exp_to_string(Fn (farg, ty1, ty2, e1)) ^ "\n\t" )
            end
      | Fun (f, farg, ty1, ty2, e1) => 
           let 
                val typeExp = checkTypeExp(e1, typeEnvAdd(f, ARROW(ty1, ty2), typeEnvAdd(farg, ty1, env)))
            in
                if(ty2=typeExp) then TypeEnv(typeEnvAdd(f, ARROW(ty1, typeExp), env)) else raise Fail ("\n" ^ "Type Error: Mismatch in declared type and actual type" ^ "\n\t" ^ "\tdeclared type: " ^ type_to_string(ty2) ^ "\n\t" ^ "\tactual type: " ^ type_to_string(typeExp) ^ "\n\t" ^ "in epression" ^ "\n\t" ^ "\t" ^ exp_to_string(Fun (f, farg, ty1, ty2, e1)) ^ "\n\t" )
            end
      | AppExp (e1, e2) => 
               (case (checkTypeExp(e1, env), checkTypeExp(e2, env)) of
                    (ARROW (t1, t2), t3) => 
                              if (t3=t1) then t2 else raise Fail ("\n" ^ "Type Error: Application arguement type mismatch" ^ "\n\t" ^ "expected arguement type: " ^ type_to_string(t1) ^ "\n\t" ^ "actual arguement type: " ^ type_to_string(t3) ^ "\n\t" ^ "in expression: " ^ "\n\t\t" ^ exp_to_string(AppExp (e1, e2)) ^ "\n\t" )
                  | (t,_) => raise Fail ("\n" ^ "Type Error: Function was expected" ^"\n\t" ^ "Type: " ^ type_to_string(t) ^ "\n\t" ^ "in expression: " ^ "\n\t\t" ^ exp_to_string(AppExp (e1, e2)) ^ "\n\t" ))
      | IfExp (e1, e2, e3)  => 
      	   (case checkTypeExp(e1, env) of
                BOOL => 
                     let
                         val typeExp2 = checkTypeExp(e2, env)
                         val typeExp3 = checkTypeExp(e3, env)
                     in
                         if (typeExp2=typeExp3) then typeExp2 else raise Fail ("\n" ^ "Type Error: Type of if branches do not match" ^ "\n\t" ^ "then branch type: " ^ type_to_string(typeExp2) ^ "\n\t" ^ "else branch type: " ^ type_to_string(typeExp3) ^ "\n\t" ^ "in expression: " ^ "\n\t\t" ^ exp_to_string(IfExp (e1, e2, e3)) ^ "\n\t" )
                     end
                | t        => raise Fail ("\n" ^ "Type Error: Test Expression in if is not of type bool" ^ "\n\t" ^ "test expression type: "^ type_to_string(t) ^ "\n\t" ^ "in expression: " ^ "\n\t\t" ^ exp_to_string(IfExp (e1, e2, e3)) ^ "\n\t" ))
      | LetExp(ValDecl(x, e1), e2)  =>
  	let
	    val v1 = checkTypeExp (e1, env)
	in
	    checkTypeExp(e2, typeEnvAdd (x, v1, env))
        end		   
and
checkTypeBinExp(b:binop, e1:exp, e2:exp, env:typeenvironment): typ =
case (b, checkTypeExp(e1, env), checkTypeExp(e2, env))  of
    (Plus, INT, INT) => INT
  |   (Minus, INT, INT) => INT
  |   (Times, INT, INT) => INT
  |   (_, t1, t2)  => raise Fail ("\n" ^ "Type Error: Operator and Operand Types do not agree" ^ "\n\t" ^ "Operator domain: " ^ "int" ^ "*" ^ "int" ^ "\n\t" ^ "Operand types: " ^ type_to_string(t1) ^ "*" ^ type_to_string(t2) ^ "\n\t" ^ "in expression: " ^ "\n\t\t" ^exp_to_string(BinExp(b, e1, e2)) ^ "\n\t" )
and
checkTypeUnExp(u:unop, e:exp, env:typeenvironment): typ=
case(u, checkTypeExp(e, env)) of 
    (Not, BOOL) => BOOL
    | (Negate, INT) => INT 
    | (Not, t) => raise Fail ("\n" ^ "Type Error: Operator and Operand Types do not agree" ^ "\n\t" ^ "Operator domain: " ^ "bool" ^ "\n\t" ^ "Operand type: " ^ type_to_string(t) ^ "\n\t" ^ "in expression: " ^ "\n\t\t" ^exp_to_string(UnExp(u, e)) ^ "\n\t" )
    | (Negate, t) => raise Fail ("\n" ^ "Type Error: Operator and Operand Types do not agree" ^ "\n\t" ^ "Operator domain: " ^ "int" ^ "\n\t" ^ "Operand type: " ^ type_to_string(t) ^ "\n\t" ^ "in expression: " ^ "\n\t\t" ^exp_to_string(UnExp(u, e)) ^ "\n\t" ) 
and
checkTypeLogicExp(l:logicop, e1:exp, e2:exp, env:typeenvironment): typ =
case (l, checkTypeExp(e1, env), checkTypeExp(e2, env))  of
     (And, BOOL, BOOL) => BOOL
  |   (Or, BOOL, BOOL) => BOOL
  |   (Xor, BOOL, BOOL) => BOOL
  |   (Implies, BOOL, BOOL) => BOOL
  |   (_, t1, t2)  => raise Fail ("\n" ^ "Type Error: Operator and Operand Types do not agree" ^ "\n\t" ^ "Operator domain: " ^ "bool" ^ "*" ^ "bool" ^ "\n\t" ^ "Operand types: " ^ type_to_string(t1) ^ "*" ^ type_to_string(t2) ^ "\n\t" ^ "in expression: " ^ "\n\t\t" ^exp_to_string(LogicExp(l, e1, e2)) ^ "\n\t" )
and		
checkTypeRelExp(r:relop, e1:exp, e2:exp, env:typeenvironment): typ =
case (r, checkTypeExp(e1, env), checkTypeExp(e2, env))  of
     (Equals, INT, INT)  => BOOL
  |   (Equals, BOOL, BOOL)  => BOOL
  |   (LessThan, INT, INT) => BOOL
  |   (GreaterThan, INT, INT) => BOOL
  |   (Equals, t1, t2)  => raise Fail ("\n" ^ "Type Error: Operator and Operand Types do not agree" ^ "\n\t" ^ "Operator domain: " ^ "int" ^ "*" ^ "int" ^ " or " ^ "bool" ^ "*" ^ "bool" ^ "\n\t" ^ "Operand types: " ^ type_to_string(t1) ^ "*" ^ type_to_string(t2) ^ "\n\t" ^ "in expression: " ^ "\n\t\t" ^exp_to_string(RelExp(r, e1, e2)) ^ "\n\t" )
  |   (_, t1, t2)  => raise Fail ("\n" ^ "Type Error: Operator and Operand Types do not agree" ^ "\n\t" ^ "Operator domain: " ^ "int" ^ "*" ^ "int" ^ "\n\t" ^ "Operand types: " ^ type_to_string(t1) ^ "*" ^ type_to_string(t2) ^ "\n\t" ^ "in expression: " ^ "\n\t\t" ^exp_to_string(RelExp(r, e1, e2)) ^ "\n\t" )	    
end
