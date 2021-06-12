%%
%name Calc
%eop EOF
%pos int
%term  ID of string | CONST of string | LPAREN | RPAREN | NOT | AND | OR  | XOR | EQUALS | IMPLIES | IF | THEN | ELSE | FI | TERM | NUM of int | PLUS | TIMES | MINUS | EQ | LESSTHAN | GREATERTHAN | NEGATE | LET | IN | END | COLON | FUN | FN | ARROW | DARROW | INT | STRING | BOOL | EOF 
%nonterm Start of AST.prg | program of AST.prg | statement of AST.exp | formula of AST.exp | DECL of AST.decl | Type of AST.typ
%noshift EOF

%right LET IN END IF THEN ELSE FI
%right FUN FN ARROW 
%nonassoc DARROW
%right IMPLIES
%left GREATERTHAN
%left LESSTHAN
%left AND OR XOR EQUALS PLUS MINUS
%left TIMES
%nonassoc EQ
%right NOT NEGATE
%nonassoc LPAREN RPAREN
%start Start
%verbose
%%
  Start   : program (program)
  program : statement program (AST.Program(statement,program))
          | formula (AST.LastExp(formula))
  statement: formula TERM (formula)
  DECL    : ID EQ formula (AST.ValDecl(ID, formula))
  Type    : INT (AST.INT)
          | STRING (AST.STRING)
          | BOOL (AST.BOOL)
          | Type ARROW Type (AST.ARROW(Type1,Type2))
  formula : ID (AST.VarExp(ID))
          | CONST (AST.ConstExp(CONST))
          | NUM (AST.NumExp(NUM))
          | NOT formula (AST.UnExp(AST.Not, formula))
          | formula AND formula (AST.LogicExp(AST.And, formula1,  formula2))        
          | formula OR formula (AST.LogicExp(AST.Or, formula1,  formula2))
          | formula XOR formula (AST.LogicExp(AST.Xor, formula1,  formula2))
          | formula EQUALS formula (AST.RelExp(AST.Equals,  formula1, formula2))
          | formula IMPLIES formula (AST.LogicExp(AST.Implies, formula1,  formula2))
          | LET DECL IN formula END (AST.LetExp(DECL, formula))
          | IF formula THEN formula ELSE formula FI (AST.IfExp(formula1, formula2, formula3))
          | LPAREN formula RPAREN (formula)
          | formula PLUS formula (AST.BinExp(AST.Plus, formula1,  formula2))
          | formula MINUS  formula (AST.BinExp(AST.Minus,  formula1,  formula2))
          | formula TIMES  formula (AST.BinExp(AST.Times,  formula1, formula2))
          | formula LESSTHAN  formula (AST.RelExp(AST.LessThan,  formula1, formula2))
          | formula GREATERTHAN  formula (AST.RelExp(AST.GreaterThan, formula1, formula2))
          | NEGATE formula (AST.UnExp(AST.Neg, formula))
          | FN LPAREN ID COLON Type RPAREN COLON Type DARROW formula (AST.Fn(ID,Type1,Type2,formula))
          | FUN ID LPAREN ID COLON Type RPAREN COLON Type DARROW formula (AST.Fun(ID1,ID2,Type1,Type2,formula))
          | LPAREN formula formula RPAREN (AST.AppExp(formula1,formula2))
