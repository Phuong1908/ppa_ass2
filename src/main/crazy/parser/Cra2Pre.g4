grammar Cra2Pre;

options {
    language=Python3;
}

@lexer::header {
from lexererr import *
}


program	returns [String s] 
	: PROGRAM ID SEMICOLON v=var_decl  c=comp_stmt DOT EOF
	  {$s = "[[" + $v.s + "],[]," + $c.s + "]. " }
	;

var_decl returns [String s] 
	: VAR ID COLON t=ctype SEMICOLON {$s = 'var('+ $ID.text+ ',' + $t.s +')'}
	| {$s = ""}
	;
	
ctype returns [String s]
	: INTEGER {$s = "integer" }
	| REAL	  {$s = "real" }
	| BOOLEAN {$s = "boolean" }	
	;
	

comp_stmt returns [String s]
	: BEGIN {t='['} (v=stmt {t += $v.s})? END {$s = t + "]"}
	;
	
stmt returns [String s]
	: t=comp_stmt {$s = $t.s}
	| t1=assi_stmt {$s = $t1.s}
	;

assi_stmt returns [String s] 
	: ID ASSOPE e=expr SEMICOLON {$s = "assign("+ $ID.text + "," + $e.s +")"}
	;
	
expr	returns [String s] 
	: INTLIT {$s = $INTLIT.text}
	;

PROGRAM		: 'program' ;
VAR		: 'var';
BEGIN		: 'begin' ;
END		: 'end' ;
BOOLEAN		: 'boolean';
INTEGER		: 'integer' ;
REAL		: 'real' ;
ASSOPE		: ':=' ;
DOT		: '.' ;
SEMICOLON 	: ';' ;
COLON		: ':' ;	
INTLIT		:[0-9]+ ;
ID    		: [a-z]+;    
WS : [ \t\r\n]+ -> skip ;

ERROR : . {raise ErrorToken(self.text)};
