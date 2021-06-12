functor CalcLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Calc_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\015\000\002\000\014\000\003\000\013\000\004\000\052\000\
\\005\000\012\000\006\000\026\000\007\000\025\000\008\000\024\000\
\\009\000\023\000\010\000\022\000\011\000\011\000\016\000\010\000\
\\017\000\020\000\018\000\019\000\019\000\018\000\021\000\017\000\
\\022\000\016\000\023\000\009\000\024\000\008\000\028\000\007\000\
\\029\000\006\000\000\000\
\\001\000\001\000\015\000\002\000\014\000\003\000\013\000\005\000\012\000\
\\011\000\011\000\016\000\010\000\023\000\009\000\024\000\008\000\
\\028\000\007\000\029\000\006\000\000\000\
\\001\000\001\000\029\000\000\000\
\\001\000\001\000\031\000\000\000\
\\001\000\001\000\046\000\000\000\
\\001\000\001\000\054\000\000\000\
\\001\000\003\000\028\000\000\000\
\\001\000\003\000\047\000\000\000\
\\001\000\004\000\058\000\006\000\026\000\007\000\025\000\008\000\024\000\
\\009\000\023\000\010\000\022\000\017\000\020\000\018\000\019\000\
\\019\000\018\000\021\000\017\000\022\000\016\000\000\000\
\\001\000\004\000\067\000\030\000\066\000\000\000\
\\001\000\004\000\072\000\030\000\066\000\000\000\
\\001\000\006\000\026\000\007\000\025\000\008\000\024\000\009\000\023\000\
\\010\000\022\000\012\000\050\000\017\000\020\000\018\000\019\000\
\\019\000\018\000\021\000\017\000\022\000\016\000\000\000\
\\001\000\006\000\026\000\007\000\025\000\008\000\024\000\009\000\023\000\
\\010\000\022\000\013\000\065\000\017\000\020\000\018\000\019\000\
\\019\000\018\000\021\000\017\000\022\000\016\000\000\000\
\\001\000\006\000\026\000\007\000\025\000\008\000\024\000\009\000\023\000\
\\010\000\022\000\014\000\073\000\017\000\020\000\018\000\019\000\
\\019\000\018\000\021\000\017\000\022\000\016\000\000\000\
\\001\000\006\000\026\000\007\000\025\000\008\000\024\000\009\000\023\000\
\\010\000\022\000\017\000\020\000\018\000\019\000\019\000\018\000\
\\021\000\017\000\022\000\016\000\026\000\064\000\000\000\
\\001\000\020\000\049\000\000\000\
\\001\000\025\000\048\000\000\000\
\\001\000\027\000\053\000\000\000\
\\001\000\027\000\063\000\000\000\
\\001\000\027\000\071\000\000\000\
\\001\000\027\000\075\000\000\000\
\\001\000\030\000\066\000\031\000\076\000\000\000\
\\001\000\030\000\066\000\031\000\079\000\000\000\
\\001\000\032\000\062\000\033\000\061\000\034\000\060\000\000\000\
\\001\000\035\000\000\000\000\000\
\\082\000\000\000\
\\083\000\000\000\
\\084\000\006\000\026\000\007\000\025\000\008\000\024\000\009\000\023\000\
\\010\000\022\000\015\000\021\000\017\000\020\000\018\000\019\000\
\\019\000\018\000\021\000\017\000\022\000\016\000\000\000\
\\085\000\000\000\
\\086\000\006\000\026\000\007\000\025\000\008\000\024\000\009\000\023\000\
\\010\000\022\000\017\000\020\000\018\000\019\000\019\000\018\000\
\\021\000\017\000\022\000\016\000\000\000\
\\087\000\000\000\
\\088\000\000\000\
\\089\000\000\000\
\\090\000\030\000\066\000\000\000\
\\091\000\000\000\
\\092\000\000\000\
\\093\000\000\000\
\\094\000\000\000\
\\095\000\018\000\019\000\000\000\
\\096\000\018\000\019\000\000\000\
\\097\000\018\000\019\000\000\000\
\\098\000\018\000\019\000\000\000\
\\099\000\006\000\026\000\007\000\025\000\008\000\024\000\009\000\023\000\
\\010\000\022\000\017\000\020\000\018\000\019\000\019\000\018\000\
\\021\000\017\000\022\000\016\000\000\000\
\\100\000\000\000\
\\101\000\000\000\
\\102\000\000\000\
\\103\000\018\000\019\000\000\000\
\\104\000\018\000\019\000\000\000\
\\105\000\000\000\
\\106\000\006\000\026\000\007\000\025\000\008\000\024\000\009\000\023\000\
\\017\000\020\000\018\000\019\000\019\000\018\000\000\000\
\\107\000\006\000\026\000\007\000\025\000\008\000\024\000\009\000\023\000\
\\017\000\020\000\018\000\019\000\019\000\018\000\021\000\017\000\000\000\
\\108\000\000\000\
\\109\000\006\000\026\000\007\000\025\000\008\000\024\000\009\000\023\000\
\\010\000\022\000\017\000\020\000\018\000\019\000\019\000\018\000\
\\021\000\017\000\022\000\016\000\000\000\
\\110\000\006\000\026\000\007\000\025\000\008\000\024\000\009\000\023\000\
\\010\000\022\000\017\000\020\000\018\000\019\000\019\000\018\000\
\\021\000\017\000\022\000\016\000\000\000\
\\111\000\000\000\
\"
val actionRowNumbers =
"\001\000\027\000\001\000\025\000\
\\006\000\002\000\003\000\001\000\
\\036\000\001\000\001\000\001\000\
\\035\000\034\000\001\000\001\000\
\\001\000\001\000\001\000\028\000\
\\001\000\001\000\001\000\001\000\
\\001\000\026\000\004\000\007\000\
\\016\000\015\000\051\000\011\000\
\\037\000\000\000\050\000\049\000\
\\047\000\048\000\046\000\042\000\
\\041\000\040\000\039\000\038\000\
\\017\000\005\000\001\000\001\000\
\\001\000\008\000\045\000\023\000\
\\018\000\014\000\029\000\012\000\
\\054\000\009\000\032\000\031\000\
\\030\000\023\000\043\000\001\000\
\\023\000\019\000\010\000\013\000\
\\033\000\023\000\020\000\044\000\
\\021\000\023\000\001\000\022\000\
\\052\000\001\000\053\000\024\000"
val gotoT =
"\
\\001\000\079\000\002\000\003\000\003\000\002\000\004\000\001\000\000\000\
\\000\000\
\\002\000\025\000\003\000\002\000\004\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\028\000\000\000\
\\004\000\030\000\000\000\
\\000\000\
\\004\000\031\000\000\000\
\\004\000\032\000\000\000\
\\004\000\033\000\000\000\
\\000\000\
\\000\000\
\\004\000\034\000\000\000\
\\004\000\035\000\000\000\
\\004\000\036\000\000\000\
\\004\000\037\000\000\000\
\\004\000\038\000\000\000\
\\000\000\
\\004\000\039\000\000\000\
\\004\000\040\000\000\000\
\\004\000\041\000\000\000\
\\004\000\042\000\000\000\
\\004\000\043\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\049\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\053\000\000\000\
\\004\000\054\000\000\000\
\\004\000\055\000\000\000\
\\000\000\
\\000\000\
\\006\000\057\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\066\000\000\000\
\\000\000\
\\004\000\067\000\000\000\
\\006\000\068\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\072\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\075\000\000\000\
\\004\000\076\000\000\000\
\\000\000\
\\000\000\
\\004\000\078\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 80
val numrules = 30
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | NUM of unit ->  (int) | CONST of unit ->  (string)
 | ID of unit ->  (string) | Type of unit ->  (AST.typ)
 | DECL of unit ->  (AST.decl) | formula of unit ->  (AST.exp)
 | statement of unit ->  (AST.exp) | program of unit ->  (AST.prg)
 | Start of unit ->  (AST.prg)
end
type svalue = MlyValue.svalue
type result = AST.prg
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 34) => true | _ => false
val showTerminal =
fn (T 0) => "ID"
  | (T 1) => "CONST"
  | (T 2) => "LPAREN"
  | (T 3) => "RPAREN"
  | (T 4) => "NOT"
  | (T 5) => "AND"
  | (T 6) => "OR"
  | (T 7) => "XOR"
  | (T 8) => "EQUALS"
  | (T 9) => "IMPLIES"
  | (T 10) => "IF"
  | (T 11) => "THEN"
  | (T 12) => "ELSE"
  | (T 13) => "FI"
  | (T 14) => "TERM"
  | (T 15) => "NUM"
  | (T 16) => "PLUS"
  | (T 17) => "TIMES"
  | (T 18) => "MINUS"
  | (T 19) => "EQ"
  | (T 20) => "LESSTHAN"
  | (T 21) => "GREATERTHAN"
  | (T 22) => "NEGATE"
  | (T 23) => "LET"
  | (T 24) => "IN"
  | (T 25) => "END"
  | (T 26) => "COLON"
  | (T 27) => "FUN"
  | (T 28) => "FN"
  | (T 29) => "ARROW"
  | (T 30) => "DARROW"
  | (T 31) => "INT"
  | (T 32) => "STRING"
  | (T 33) => "BOOL"
  | (T 34) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28)
 $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21)
 $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 14) $$ (T 13)
 $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ 
(T 5) $$ (T 4) $$ (T 3) $$ (T 2)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.program program1, program1left, 
program1right)) :: rest671)) => let val  result = MlyValue.Start (fn _
 => let val  (program as program1) = program1 ()
 in (program)
end)
 in ( LrTable.NT 0, ( result, program1left, program1right), rest671)

end
|  ( 1, ( ( _, ( MlyValue.program program1, _, program1right)) :: ( _,
 ( MlyValue.statement statement1, statement1left, _)) :: rest671)) =>
 let val  result = MlyValue.program (fn _ => let val  (statement as 
statement1) = statement1 ()
 val  (program as program1) = program1 ()
 in (AST.Program(statement,program))
end)
 in ( LrTable.NT 1, ( result, statement1left, program1right), rest671)

end
|  ( 2, ( ( _, ( MlyValue.formula formula1, formula1left, 
formula1right)) :: rest671)) => let val  result = MlyValue.program (fn
 _ => let val  (formula as formula1) = formula1 ()
 in (AST.LastExp(formula))
end)
 in ( LrTable.NT 1, ( result, formula1left, formula1right), rest671)

end
|  ( 3, ( ( _, ( _, _, TERM1right)) :: ( _, ( MlyValue.formula 
formula1, formula1left, _)) :: rest671)) => let val  result = 
MlyValue.statement (fn _ => let val  (formula as formula1) = formula1
 ()
 in (formula)
end)
 in ( LrTable.NT 2, ( result, formula1left, TERM1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.formula formula1, _, formula1right)) :: _ ::
 ( _, ( MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result
 = MlyValue.DECL (fn _ => let val  (ID as ID1) = ID1 ()
 val  (formula as formula1) = formula1 ()
 in (AST.ValDecl(ID, formula))
end)
 in ( LrTable.NT 4, ( result, ID1left, formula1right), rest671)
end
|  ( 5, ( ( _, ( _, INT1left, INT1right)) :: rest671)) => let val  
result = MlyValue.Type (fn _ => (AST.INT))
 in ( LrTable.NT 5, ( result, INT1left, INT1right), rest671)
end
|  ( 6, ( ( _, ( _, STRING1left, STRING1right)) :: rest671)) => let
 val  result = MlyValue.Type (fn _ => (AST.STRING))
 in ( LrTable.NT 5, ( result, STRING1left, STRING1right), rest671)
end
|  ( 7, ( ( _, ( _, BOOL1left, BOOL1right)) :: rest671)) => let val  
result = MlyValue.Type (fn _ => (AST.BOOL))
 in ( LrTable.NT 5, ( result, BOOL1left, BOOL1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.Type Type2, _, Type2right)) :: _ :: ( _, ( 
MlyValue.Type Type1, Type1left, _)) :: rest671)) => let val  result = 
MlyValue.Type (fn _ => let val  Type1 = Type1 ()
 val  Type2 = Type2 ()
 in (AST.ARROW(Type1,Type2))
end)
 in ( LrTable.NT 5, ( result, Type1left, Type2right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.formula (fn _ => let val  (ID as ID1) = 
ID1 ()
 in (AST.VarExp(ID))
end)
 in ( LrTable.NT 3, ( result, ID1left, ID1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.CONST CONST1, CONST1left, CONST1right)) :: 
rest671)) => let val  result = MlyValue.formula (fn _ => let val  (
CONST as CONST1) = CONST1 ()
 in (AST.ConstExp(CONST))
end)
 in ( LrTable.NT 3, ( result, CONST1left, CONST1right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.NUM NUM1, NUM1left, NUM1right)) :: rest671)
) => let val  result = MlyValue.formula (fn _ => let val  (NUM as NUM1
) = NUM1 ()
 in (AST.NumExp(NUM))
end)
 in ( LrTable.NT 3, ( result, NUM1left, NUM1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.formula formula1, _, formula1right)) :: ( _
, ( _, NOT1left, _)) :: rest671)) => let val  result = 
MlyValue.formula (fn _ => let val  (formula as formula1) = formula1 ()
 in (AST.UnExp(AST.Not, formula))
end)
 in ( LrTable.NT 3, ( result, NOT1left, formula1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: _
 :: ( _, ( MlyValue.formula formula1, formula1left, _)) :: rest671))
 => let val  result = MlyValue.formula (fn _ => let val  formula1 = 
formula1 ()
 val  formula2 = formula2 ()
 in (AST.LogicExp(AST.And, formula1,  formula2))
end)
 in ( LrTable.NT 3, ( result, formula1left, formula2right), rest671)

end
|  ( 14, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: _
 :: ( _, ( MlyValue.formula formula1, formula1left, _)) :: rest671))
 => let val  result = MlyValue.formula (fn _ => let val  formula1 = 
formula1 ()
 val  formula2 = formula2 ()
 in (AST.LogicExp(AST.Or, formula1,  formula2))
end)
 in ( LrTable.NT 3, ( result, formula1left, formula2right), rest671)

end
|  ( 15, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: _
 :: ( _, ( MlyValue.formula formula1, formula1left, _)) :: rest671))
 => let val  result = MlyValue.formula (fn _ => let val  formula1 = 
formula1 ()
 val  formula2 = formula2 ()
 in (AST.LogicExp(AST.Xor, formula1,  formula2))
end)
 in ( LrTable.NT 3, ( result, formula1left, formula2right), rest671)

end
|  ( 16, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: _
 :: ( _, ( MlyValue.formula formula1, formula1left, _)) :: rest671))
 => let val  result = MlyValue.formula (fn _ => let val  formula1 = 
formula1 ()
 val  formula2 = formula2 ()
 in (AST.RelExp(AST.Equals,  formula1, formula2))
end)
 in ( LrTable.NT 3, ( result, formula1left, formula2right), rest671)

end
|  ( 17, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: _
 :: ( _, ( MlyValue.formula formula1, formula1left, _)) :: rest671))
 => let val  result = MlyValue.formula (fn _ => let val  formula1 = 
formula1 ()
 val  formula2 = formula2 ()
 in (AST.LogicExp(AST.Implies, formula1,  formula2))
end)
 in ( LrTable.NT 3, ( result, formula1left, formula2right), rest671)

end
|  ( 18, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.formula 
formula1, _, _)) :: _ :: ( _, ( MlyValue.DECL DECL1, _, _)) :: ( _, (
 _, LET1left, _)) :: rest671)) => let val  result = MlyValue.formula
 (fn _ => let val  (DECL as DECL1) = DECL1 ()
 val  (formula as formula1) = formula1 ()
 in (AST.LetExp(DECL, formula))
end)
 in ( LrTable.NT 3, ( result, LET1left, END1right), rest671)
end
|  ( 19, ( ( _, ( _, _, FI1right)) :: ( _, ( MlyValue.formula formula3
, _, _)) :: _ :: ( _, ( MlyValue.formula formula2, _, _)) :: _ :: ( _,
 ( MlyValue.formula formula1, _, _)) :: ( _, ( _, IF1left, _)) :: 
rest671)) => let val  result = MlyValue.formula (fn _ => let val  
formula1 = formula1 ()
 val  formula2 = formula2 ()
 val  formula3 = formula3 ()
 in (AST.IfExp(formula1, formula2, formula3))
end)
 in ( LrTable.NT 3, ( result, IF1left, FI1right), rest671)
end
|  ( 20, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.formula 
formula1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let
 val  result = MlyValue.formula (fn _ => let val  (formula as formula1
) = formula1 ()
 in (formula)
end)
 in ( LrTable.NT 3, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: _
 :: ( _, ( MlyValue.formula formula1, formula1left, _)) :: rest671))
 => let val  result = MlyValue.formula (fn _ => let val  formula1 = 
formula1 ()
 val  formula2 = formula2 ()
 in (AST.BinExp(AST.Plus, formula1,  formula2))
end)
 in ( LrTable.NT 3, ( result, formula1left, formula2right), rest671)

end
|  ( 22, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: _
 :: ( _, ( MlyValue.formula formula1, formula1left, _)) :: rest671))
 => let val  result = MlyValue.formula (fn _ => let val  formula1 = 
formula1 ()
 val  formula2 = formula2 ()
 in (AST.BinExp(AST.Minus,  formula1,  formula2))
end)
 in ( LrTable.NT 3, ( result, formula1left, formula2right), rest671)

end
|  ( 23, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: _
 :: ( _, ( MlyValue.formula formula1, formula1left, _)) :: rest671))
 => let val  result = MlyValue.formula (fn _ => let val  formula1 = 
formula1 ()
 val  formula2 = formula2 ()
 in (AST.BinExp(AST.Times,  formula1, formula2))
end)
 in ( LrTable.NT 3, ( result, formula1left, formula2right), rest671)

end
|  ( 24, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: _
 :: ( _, ( MlyValue.formula formula1, formula1left, _)) :: rest671))
 => let val  result = MlyValue.formula (fn _ => let val  formula1 = 
formula1 ()
 val  formula2 = formula2 ()
 in (AST.RelExp(AST.LessThan,  formula1, formula2))
end)
 in ( LrTable.NT 3, ( result, formula1left, formula2right), rest671)

end
|  ( 25, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: _
 :: ( _, ( MlyValue.formula formula1, formula1left, _)) :: rest671))
 => let val  result = MlyValue.formula (fn _ => let val  formula1 = 
formula1 ()
 val  formula2 = formula2 ()
 in (AST.RelExp(AST.GreaterThan, formula1, formula2))
end)
 in ( LrTable.NT 3, ( result, formula1left, formula2right), rest671)

end
|  ( 26, ( ( _, ( MlyValue.formula formula1, _, formula1right)) :: ( _
, ( _, NEGATE1left, _)) :: rest671)) => let val  result = 
MlyValue.formula (fn _ => let val  (formula as formula1) = formula1 ()
 in (AST.UnExp(AST.Neg, formula))
end)
 in ( LrTable.NT 3, ( result, NEGATE1left, formula1right), rest671)

end
|  ( 27, ( ( _, ( MlyValue.formula formula1, _, formula1right)) :: _
 :: ( _, ( MlyValue.Type Type2, _, _)) :: _ :: _ :: ( _, ( 
MlyValue.Type Type1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) ::
 _ :: ( _, ( _, FN1left, _)) :: rest671)) => let val  result = 
MlyValue.formula (fn _ => let val  (ID as ID1) = ID1 ()
 val  Type1 = Type1 ()
 val  Type2 = Type2 ()
 val  (formula as formula1) = formula1 ()
 in (AST.Fn(ID,Type1,Type2,formula))
end)
 in ( LrTable.NT 3, ( result, FN1left, formula1right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.formula formula1, _, formula1right)) :: _
 :: ( _, ( MlyValue.Type Type2, _, _)) :: _ :: _ :: ( _, ( 
MlyValue.Type Type1, _, _)) :: _ :: ( _, ( MlyValue.ID ID2, _, _)) ::
 _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, FUN1left, _)) :: 
rest671)) => let val  result = MlyValue.formula (fn _ => let val  ID1
 = ID1 ()
 val  ID2 = ID2 ()
 val  Type1 = Type1 ()
 val  Type2 = Type2 ()
 val  (formula as formula1) = formula1 ()
 in (AST.Fun(ID1,ID2,Type1,Type2,formula))
end)
 in ( LrTable.NT 3, ( result, FUN1left, formula1right), rest671)
end
|  ( 29, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.formula 
formula2, _, _)) :: ( _, ( MlyValue.formula formula1, _, _)) :: ( _, (
 _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.formula (fn _ => let val  formula1 = formula1 ()
 val  formula2 = formula2 ()
 in (AST.AppExp(formula1,formula2))
end)
 in ( LrTable.NT 3, ( result, LPAREN1left, RPAREN1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.Start x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Calc_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun CONST (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.CONST (fn () => i),p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun XOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun EQUALS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun IMPLIES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun FI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun TERM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun NUM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.NUM (fn () => i),p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun LESSTHAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun GREATERTHAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun NEGATE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun FUN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun FN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun ARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun DARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun STRING (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
end
end
