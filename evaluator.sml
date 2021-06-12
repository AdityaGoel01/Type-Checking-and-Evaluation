structure EVALUATOR  =
struct
open AST

fun evalPrg(p:prg, env:environment) =
    case p of 
        Program (e, p1) => 
                (case evalExp(e, env) of 
                      Env env1 => evalPrg(p1, env1)
                    | IntVal v => (print(Int.toString(v)^" : int\n"); evalPrg(p1, env))
                    | BoolVal v => (if v then print("TRUE : bool\n") else print ("FALSE : bool\n"); evalPrg(p1, env)))
      | LastExp (e)         => 
                (case evalExp(e, env) of 
                      Env env1 => print("")
                    | IntVal v => print(Int.toString(v)^" : int\n")
                    | BoolVal v => if v then print("TRUE : bool\n") else print ("FALSE : bool\n"))      
and evalExp(e:exp, env:environment): value =
    case e of
	NumExp i            => IntVal i
      | ConstExp s         => BoolVal (s="TRUE")
      | VarExp x            => envLookup (x, env) 				  
      | BinExp (b, e1, e2)  => evalBinExp(b, e1, e2, env)
      | UnExp(u, e)         => evalUnExp(u, e, env)
      | LogicExp(l, e1, e2) => evalLogicExp(l, e1, e2, env)
      | RelExp(r, e1, e2)   => evalRelExp(r, e1, e2, env)
      | Fn (farg, ty1, ty2, e1) => FunVal(farg, e1, env)
      | Fun (f, farg, ty1, ty2, e1) => 
           let
                 val fval = FunVal(farg, e1, env)
           in
                 Env(envAdd(f, fval, env))
           end
      | AppExp (e1, e2) =>
           let 
                 val FunVal(farg, body, envClosure) = evalExp(e1, env);
                 val argVal = evalExp(e2, env);
           in
                 evalExp(body , envAdd(farg, argVal, envClosure @ env))
           end    
      | IfExp (e1, e2, e3)  => 
      	   let
      	        val BoolVal(b) = evalExp(e1, env)
           in 
                if b then evalExp(e2, env) else evalExp(e3, env)
           end
      | LetExp(ValDecl(x, e1), e2)  =>
  	let
	    val v1 = evalExp (e1, env)
	in
	    evalExp(e2, envAdd (x, v1, env))
        end		   
and
evalBinExp(b:binop, e1:exp, e2:exp, env:environment):value =
case (b, evalExp(e1, env), evalExp(e2, env))  of
    (Plus, IntVal i1, IntVal i2) => IntVal (i1+i2)
  |   (Minus, IntVal i1, IntVal i2) => IntVal (i1-i2)
  |   (Times, IntVal i1, IntVal i2) => IntVal (i1*i2)
and
evalUnExp(u:unop, e:exp, env:environment):value=
case(u, evalExp(e, env)) of 
    (Not, BoolVal i) => BoolVal (not(i))
    | (Negate, IntVal i) => IntVal (~i)
and
evalLogicExp(l:logicop, e1:exp, e2:exp, env:environment):value =
case (l, evalExp(e1, env), evalExp(e2, env))  of
     (And, BoolVal i1, BoolVal i2) => BoolVal (i1 andalso i2)
  |   (Or, BoolVal i1, BoolVal i2) => BoolVal (i1 orelse i2)
  |   (Xor, BoolVal i1, BoolVal i2) => BoolVal ((i1 andalso not (i2)) orelse (not (i1) andalso i2))
  |   (Implies, BoolVal i1, BoolVal i2) => BoolVal (not (i1 andalso not (i2)))
and		
evalRelExp(r:relop, e1:exp, e2:exp, env:environment):value =
case (r, evalExp(e1, env), evalExp(e2, env))  of
     (Equals, IntVal i1, IntVal i2)  => BoolVal (i1 = i2)
  |   (Equals, BoolVal i1, BoolVal i2)  => BoolVal (i1 = i2)
  |   (LessThan, IntVal i1, IntVal i2) => BoolVal (i1 < i2)
  |   (GreaterThan, IntVal i1, IntVal i2) => BoolVal (i1 > i2)
end
