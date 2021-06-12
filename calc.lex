structure Tokens= Tokens
  
  type pos = int
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token  
  type lexresult = (svalue, pos) token
  val str = ref ""
  val str1 = ref ""
  val pos = ref 0
  val eof = fn () => Tokens.EOF(!pos, !pos)
  val error = fn (e, _) => TextIO.output(TextIO.stdOut,e ^ "\n")
  val linenum = ref 1
  val colnum = ref 1
  fun revfold _ nil b = b
  | revfold f (hd::tl) b = revfold f tl (f(hd,b))
  fun refinc x =  (x := !x + 1; !x)
  fun setone x =  (x := 1; !x)
  fun strappend s = ()
  fun init() = ()
%%
%header (functor CalcLexFun(structure Tokens:Calc_TOKENS));
letter=[a-zA-Z];
alpha={letter}[a-zA-Z0-9]*;
digit=[0-9];
ws = [\ \t];
%%
\n       => (pos := (!pos) + 1; refinc linenum; setone colnum; lex());
\r\n       => (pos := (!pos) + 1; refinc linenum; setone colnum; lex());
{ws}+    => (colnum:= (!colnum) + size(yytext); lex());
"("      => (str:= (!str)^(", LPAREN \""^yytext^"\"");colnum:= (!colnum) + size(yytext); Tokens.LPAREN(!pos,!pos));
")"      => (str:= (!str)^(", RPAREN \""^yytext^"\"");colnum:= (!colnum) + size(yytext); Tokens.RPAREN(!pos,!pos));
"TRUE"   => (str:= (!str)^(", CONST \""^yytext^"\"");colnum:= (!colnum) + size(yytext); Tokens.CONST(yytext,!pos,!pos));
"FALSE"  => (str:= (!str)^(", CONST \""^yytext^"\"");colnum:= (!colnum) + size(yytext); Tokens.CONST(yytext,!pos,!pos));
"NOT"    => (str:= (!str)^(", NOT \""^yytext^"\"");colnum:= (!colnum) + size(yytext); Tokens.NOT(!pos,!pos));
"AND"    => (str:= (!str)^(", AND \""^yytext^"\"");colnum:= (!colnum) + size(yytext); Tokens.AND(!pos,!pos));
"OR"     => (str:= (!str)^(", OR \""^yytext^"\"");colnum:= (!colnum) + size(yytext); Tokens.OR(!pos,!pos));
"XOR"    => (str:= (!str)^(", XOR \""^yytext^"\"");colnum:= (!colnum) + size(yytext); Tokens.XOR(!pos,!pos));
"EQUALS" => (str:= (!str)^(", EQUALS \""^yytext^"\"");colnum:= (!colnum) + size(yytext); Tokens.EQUALS(!pos,!pos));
"IMPLIES"=> (str:= (!str)^(", IMPLIES \""^yytext^"\"");colnum:= (!colnum) + size(yytext); Tokens.IMPLIES(!pos,!pos));
"if"     => (str:= (!str)^(", IF \""^yytext^"\"");colnum:= (!colnum) + size(yytext); Tokens.IF(!pos,!pos));
"then"   => (str:= (!str)^(", THEN \""^yytext^"\"");colnum:= (!colnum) + size(yytext); Tokens.THEN(!pos,!pos));
"else"   => (str:= (!str)^(", ELSE \""^yytext^"\"");colnum:= (!colnum) + size(yytext); Tokens.ELSE(!pos,!pos));
"fi"     => (str:= (!str)^(", FI \""^yytext^"\"");colnum:= (!colnum) + size(yytext); Tokens.FI(!pos,!pos));
"PLUS"   => (str:= (!str)^(", PLUS \""^yytext^"\"");colnum:= (!colnum) + size(yytext); Tokens.PLUS(!pos,!pos));
"TIMES"  => (str:= (!str)^(", TIMES \""^yytext^"\"");colnum:= (!colnum) + size(yytext); Tokens.TIMES(!pos,!pos));
"MINUS"  => (str:= (!str)^(", MINUS \""^yytext^"\"");colnum:= (!colnum) + size(yytext); Tokens.MINUS(!pos,!pos));
"="     => (str:= (!str)^(", EQ \""^yytext^"\"");colnum:= (!colnum) + size(yytext); Tokens.EQ(!pos,!pos));
"LESSTHAN" => (str:= (!str)^(", LESSTHAN \""^yytext^"\"");colnum:= (!colnum) + size(yytext); Tokens.LESSTHAN(!pos,!pos));
"GREATERTHAN" => (str:= (!str)^(", GREAYERTHAN \""^yytext^"\"");colnum:= (!colnum) + size(yytext); Tokens.GREATERTHAN(!pos,!pos));
"NEGATE" => (str:= (!str)^(", NEGATE \""^yytext^"\"");colnum:= (!colnum) + size(yytext); Tokens.NEGATE(!pos,!pos));
"let"    => (str:= (!str)^(", LET \""^yytext^"\"");colnum:= (!colnum) + size(yytext); Tokens.LET(!pos,!pos));
"in"     => (str:= (!str)^(", IN \""^yytext^"\"");colnum:= (!colnum) + size(yytext); Tokens.IN(!pos,!pos));
"end"    => (str:= (!str)^(", END \""^yytext^"\"");colnum:= (!colnum) + size(yytext); Tokens.END(!pos,!pos));
"fun"    => (str:= (!str)^(", FUN \""^yytext^"\"");colnum:= (!colnum) + size(yytext); Tokens.FUN(!pos,!pos));
"fn"     => (str:= (!str)^(", FN \""^yytext^"\"");colnum:= (!colnum) + size(yytext); Tokens.FN(!pos,!pos));
"->"     => (str:= (!str)^(", ARROW \""^yytext^"\"");colnum:= (!colnum) + size(yytext); Tokens.ARROW(!pos,!pos));
"=>"     => (str:= (!str)^(", DARROW \""^yytext^"\"");colnum:= (!colnum) + size(yytext); Tokens.DARROW(!pos,!pos));
"int"     => (str:= (!str)^(", INT \""^yytext^"\"");colnum:= (!colnum) + size(yytext); Tokens.INT(!pos,!pos));
"string"     => (str:= (!str)^(", STRING \""^yytext^"\"");colnum:= (!colnum) + size(yytext); Tokens.STRING(!pos,!pos));
"bool"     => (str:= (!str)^(", BOOL \""^yytext^"\"");colnum:= (!colnum) + size(yytext); Tokens.BOOL(!pos,!pos));
":"      => (str:= (!str)^(", COLON \""^yytext^"\"");colnum:= (!colnum) + size(yytext); Tokens.COLON(!pos,!pos));
";"      => (str:= (!str)^(", TERM \""^yytext^"\"");colnum:= (!colnum) + size(yytext); Tokens.TERM(!pos,!pos));
{alpha} => (str:= (!str)^(", ID \""^yytext^"\"");colnum:= (!colnum) + size(yytext); Tokens.ID(yytext,!pos,!pos));
{digit}+ => (str:= (!str)^(", NUM \""^yytext^"\"");colnum:= (!colnum) + size(yytext); Tokens.NUM(List.foldl (fn (a,r) => ord(a) - ord(#"0") + 10*r) 0 (explode yytext),!pos,!pos));
.        => (str1:= (!str1)^("Unknown Token:"^Int.toString(!linenum)^":"^Int.toString(!colnum)^":"^yytext^"\n"); colnum:= (!colnum) + size(yytext); lex());
