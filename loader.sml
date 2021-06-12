CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");
use "ast.sml";
use "typechecker.sml";
use "evaluator.sml";
use "calc.yacc.sig";
use "calc.yacc.sml";
use "calc.lex.sml";
use "load-calc.sml";
open TYPECHECKER;
open EVALUATOR;
fun typeCheckEval f = (checkTypePrg(f, []); evalPrg(f, []));
Control.Print.printLength := 1000; (* set printing parameters so that *)
Control.Print.printDepth := 1000; (* we’ll see all details *)
Control.Print.stringDepth := 1000; (* and strings *)
