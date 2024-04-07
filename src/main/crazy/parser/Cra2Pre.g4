grammar Cra2Pre;

options {
    language=Python3;
}

@lexer::header {
from lexererr import *
}

program returns [String s] : programDeclaration{$s = '[['} (varDeclList{$s = $s + $varDeclList.v})?{$s = $s + '],['} (funcDeclList{$s = $s + $funcDeclList.v})?{$s = $s + ']'} blockStatement DOT{$s = $s + f',{$blockStatement.v}]. '} EOF;
programDeclaration: PROGRAM Identifier SEMICOLON;

varDeclList returns [String v]: v1=varDecl{$v = $v1.v} (varDecl{$v = $v + ',' + $varDecl.v})*;
varDecl returns [String v]: (variableDeclaration{$v = $variableDeclaration.v} | constantDeclaration{$v = $constantDeclaration.v}) SEMICOLON;
funcDeclList returns [String v]: f1=funcDecl{$v = $f1.v} (funcDecl{$v = $v + ',' + $funcDecl.v})*;
funcDecl returns [String v]: (procedureDeclaration{$v = $procedureDeclaration.v} | functionDeclaration{$v = $functionDeclaration.v}) SEMICOLON;

constantDeclaration returns [String v]: CONST Identifier EQ constant{$v = f'const({$Identifier.text},{$constant.v})'};
constant returns [String v]: IntegerLiteral{$v = $IntegerLiteral.text}
                             | RealLiteral{$v = $RealLiteral.text} 
                             | BooleanLiteral{$v = $BooleanLiteral.text} ;
variableDeclaration returns [String v]: VAR identifierList COLON crazyType{$v = ','.join([f'var({id},{$crazyType.v})' for id in $identifierList.v])};                       

crazyType returns [String v]: primitiveType{$v = $primitiveType.v}
                            | arrayType{$v = $arrayType.v} ;
arrayType returns [String v]: ARRAY (LBRACK i1=IntegerLiteral{$v = f'arr([{$i1.text}'} RBRACK) (LBRACK IntegerLiteral{$v = $v + ',' + $IntegerLiteral.text} RBRACK)* OF arrayDeclarableType{$v = $v + f'],{$arrayDeclarableType.v})'};
arrayDeclarableType returns [String v]: INTEGER{$v = $INTEGER.text} 
                                        | REAL{$v = $REAL.text}  
                                        | BOOLEAN{$v = $BOOLEAN.text};
primitiveType returns [String v]: INTEGER{$v = $INTEGER.text} 
                                | REAL{$v = $REAL.text} 
                                | BOOLEAN{$v = $BOOLEAN.text} 
                                | STRING{$v = $STRING.text};

functionDeclaration returns [String v]: FUNCTION Identifier LPAREN paramDeclarationList? RPAREN COLON primitiveType SEMICOLON blockStatement{$v = f'func({$Identifier.text},[{$paramDeclarationList.v}],{$primitiveType.v},{$blockStatement.v})'};
procedureDeclaration returns [String v]: PROCEDURE Identifier LPAREN paramDeclarationList? RPAREN SEMICOLON blockStatement{$v = f'proc({$Identifier.text},[{$paramDeclarationList.v}],{$blockStatement.v})'};
paramDeclarationList returns [String v]: p1=paramDeclaration{$v = $p1.v} (SEMICOLON paramDeclaration{$v = $v + ',' + $paramDeclaration.v})*;
paramDeclaration returns [String v]: identifierList COLON crazyType{$v = ','.join([f'par({id},{$crazyType.v})' for id in $identifierList.v])};
identifierList returns [List v]: i1=Identifier{$v = [$i1.text]} (COMMA Identifier{$v = $v + [$Identifier.text]})*;   
statements returns [String v]: s1=statement{$v = $s1.v} (statement{$v = $v + ',' + $statement.v})*;

//Statement
statement returns [String v]: assignStatement{$v = $assignStatement.v} 
                            | blockStatement{$v = $blockStatement.v}
                            | ifStatement{$v = $ifStatement.v} 
                            | functionCallStatement{$v = $functionCallStatement.v} 
                            | whileStatement{$v = $whileStatement.v} 
                            | doWhileStatement{$v = $doWhileStatement.v} 
                            | loopDoStatement{$v = $loopDoStatement.v}  
                            | breakStatement{$v = $breakStatement.v} 
                            | continueStatement{$v = $continueStatement.v};
assignStatement returns [String v]: assignStmtLeftSide ASSOP expression SEMICOLON{$v = f'assign({$assignStmtLeftSide.v},{$expression.v})'};
assignStmtLeftSide returns [String v]: Identifier{$v = $Identifier.text} 
                                    | (Identifier subscriptionList{$v = f'ele({$Identifier.text},[{$subscriptionList.v}])'});
ifStatement returns [String v]: IF expression THEN s1=statement{$v = f'if({$expression.v},{$s1.v})'} (ELSE s2=statement{$v = f'if({$expression.v},{$s1.v},{$s2.v})'})?;
whileStatement returns [String v]: WHILE expression DO statement{$v=f'while({$expression.v},{$statement.v})'};
doWhileStatement returns [String v]: DO{$v = 'do(['} (statements{$v = $v + $statements.v})? WHILE expression SEMICOLON{$v = $v + '],' + $expression.v + ')'}; // test pass, but requirement is confusing
loopDoStatement returns [String v]: LOOP expression DO statement{$v=f'loop({$expression.v},{$statement.v})'};
breakStatement returns [String v]: BREAK SEMICOLON{$v='break(null)'};
continueStatement returns [String v]: CONTINUE SEMICOLON{$v='continue(null)'};
functionCallStatement returns [String v]: functionCall SEMICOLON{$v = $functionCall.v};
functionCall returns [String v]: Identifier{$v = f'call({$Identifier.text},['} LPAREN (expressionList{$v = $v + $expressionList.v})? RPAREN{$v = $v + '])'};
expressionList returns [String v]: e1=expression{$v = $e1.v} (COMMA expression{$v = $v + ',' + $expression.v})*;
blockStatement returns [String v]: BEGIN{$v = '['} (blockBody{$v = $v + $blockBody.v})? END{$v = $v + ']'};
blockBody returns [String v]: (localDeclaration{$v = $v + ($localDeclaration.v + ',') if $v else ($localDeclaration.v + ',')})* statements{$v = $v + $statements.v if $v else $statements.v};
localDeclaration returns [String v]: (variableDeclaration{$v = $variableDeclaration.v} | constantDeclaration{$v = $constantDeclaration.v}) SEMICOLON;

//Expression
expression returns [String v]: e1=expression PLUS multiplicativeExpression{$v = f'add({$e1.v},{$multiplicativeExpression.v})'} 
                  | e2=expression MINUS multiplicativeExpression{$v = f'sub({$e2.v},{$multiplicativeExpression.v})'} 
                  | multiplicativeExpression{$v = $multiplicativeExpression.v};

multiplicativeExpression returns [String v]: m1=multiplicativeExpression TIMES equalityExpression{$v = f'times({$m1.v},{$equalityExpression.v})'} 
                                            | m2=multiplicativeExpression DIVIDE equalityExpression{$v = f'rdiv({$m2.v},{$equalityExpression.v})'}
                                            | m3=multiplicativeExpression DIV equalityExpression{$v = f'idiv({$m3.v},{$equalityExpression.v})'} 
                                            | m4=multiplicativeExpression MOD equalityExpression{$v = f'imod({$m4.v},{$equalityExpression.v})'} 
                                            | equalityExpression{$v = $equalityExpression.v};

equalityExpression returns [String v]: e1=equalityExpression NE relationalExpression{$v = f'eql({$e1.v},{$relationalExpression.v})'}  
                                    | e2=equalityExpression EQ relationalExpression{$v = f'ne({$e2.v},{$relationalExpression.v})'} 
                                    | relationalExpression{$v = $relationalExpression.v};

relationalExpression returns [String v]: r1=relationalExpression GT logicalOrExpression{$v = f'greater({$r1.v},{$logicalOrExpression.v})'}
                                      | r2=relationalExpression LT logicalOrExpression{$v = f'less({$r2.v},{$logicalOrExpression.v})'}
                                      | r3=relationalExpression GE logicalOrExpression{$v = f'ge({$r3.v},{$logicalOrExpression.v})'} 
                                      | r4=relationalExpression LE logicalOrExpression{$v = f'le({$r4.v},{$logicalOrExpression.v})'} 
                                      | logicalOrExpression{$v = $logicalOrExpression.v};

logicalOrExpression returns [String v]: l=logicalOrExpression OR logicalAndExpression{$v = f'bor({$l.v},{$logicalAndExpression.v})'} 
                                      | logicalAndExpression{$v = $logicalAndExpression.v};

logicalAndExpression returns [String v]: l=logicalAndExpression AND unaryExpression{$v = f'band({$l.v},{$unaryExpression.v})'} 
                                        | unaryExpression{$v = $unaryExpression.v};

unaryExpression returns [String v]: NOT postfixExpression{$v = f'bnot({$postfixExpression.v})'} 
                                  | MINUS postfixExpression{$v = f'sub({$postfixExpression.v})'} 
                                  | postfixExpression{$v = $postfixExpression.v};

postfixExpression returns [String v]: Identifier subscriptionList{$v = f'ele({$Identifier.text},[{$subscriptionList.v}])'}
                                    | operand{$v = $operand.v};

subscriptionList returns [String v]: (LBRACK e1=expression{$v = $e1.v} RBRACK) (LBRACK expression{$v = $v + ',' + $expression.v} RBRACK)*;

operand returns [String v] : Identifier{$v = $Identifier.text}
                            | constant{$v = $constant.v} 
                            | StringLiteral{$v = 'str(' + $StringLiteral.text.replace("''", "<placeholder>").replace('"', '""').replace("'", '"').replace("<placeholder>", "'") + ')'}
                            | arrayLiteral{$v = $arrayLiteral.v} 
                            | functionCall{$v= $functionCall.v}  
                            | LPAREN expression RPAREN{$v = f'{$expression.v}'};

arrayLiteral returns [String v]: LBRACK arrayElement{$v = f'array([{$arrayElement.v}'} (COMMA arrayElement{$v = f'{$v},{$arrayElement.v}'})* RBRACK{$v = f'{$v}])'};
arrayElement returns [String v]: IntegerLiteral {$v = $IntegerLiteral.text} 
                              | RealLiteral {$v = $RealLiteral.text}
                              | arrayLiteral {$v = $arrayLiteral.v}; 

// Operators
// Operator: LPAREN | RPAREN | LBRACK | RBRACK | PLUS | MINUS | TIMES | DIVIDE | GT | LT | LE | GE | NE | EQ | ASSOP;
LPAREN: '(';
RPAREN: ')';
LBRACK: '[';
RBRACK: ']';
PLUS: '+';
MINUS: '-';
TIMES: '*';
DIVIDE: '/';
LE: '<=';
GE: '>=';
NE: '<>';
ASSOP: ':=';
GT: '>';
LT: '<';
EQ: '=';

// Separators
// Separator: SEMICOLON | COLON | COMMA | DOT;
SEMICOLON: ';';
COLON: ':';
COMMA: ',';
DOT: '.';

// Keywords: AND | CONTINUE | OF | THEN | ARRAY | DIV | FUNCTION | OR | BEGIN | DO | IF | LOOP | PROCEDURE | BOOLEAN | INTEGER | PROGRAM | VAR | BREAK | ELSE | MOD | REAL | WHILE | CONST | END | NOT;
AND: 'and';
CONTINUE: 'continue';
OF: 'of';
THEN: 'then';
ARRAY: 'array';
DIV: 'div';
FUNCTION: 'function';
OR: 'or';
BEGIN: 'begin';
DO: 'do';
IF: 'if';
LOOP: 'loop';
PROCEDURE: 'procedure';
BOOLEAN: 'boolean';
INTEGER: 'integer';
PROGRAM: 'program';
VAR: 'var';
BREAK: 'break';
ELSE: 'else';
MOD: 'mod';
REAL: 'real';
WHILE: 'while';
CONST: 'const';
END: 'end';
NOT: 'not';
STRING: 'string';

// Types
BooleanLiteral: 'true' | 'false';
RealLiteral: Integer Fractional Exponential? | Integer Exponential | Fractional Exponential | Fractional;
IntegerLiteral: Integer;
fragment Integer: NonZeroDigit Digit* | '0';
StringLiteral: '\'' StringCharacters? '\''; 
fragment StringCharacters: StringCharacter+;
fragment StringCharacter: ~['\\\n\t] | EscapeSequence | '\'' '\'';
fragment EscapeSequence: '\\' ['nt\\];
IllegalEscapeInString : '\'' StringCharacters? '\\' ~[tn'\\]{raise IllegalEscape(self.text)} .?;
UncloseString : '\'' StringCharacters? {raise UncloseString(self.text)};

Identifier: LowercaseLetter (LowercaseLetter | UppercaseLetter | Digit)*;
fragment LowercaseLetter: [a-z];
fragment UppercaseLetter: [A-Z];
fragment Digit: [0-9];
fragment Exponential: [eE] '-'? Digit+; 
fragment Fractional: '.' Digit* NonZeroDigit | '.0';
fragment NonZeroDigit: [1-9];


BLOCK_COMMENT: '/*' .*? '*/' -> skip;
LINE_COMMENT: '//' .*? '\n' -> skip ;
WS: [ \t\r\n]+ -> skip ;

ERROR_TOKEN : . {raise ErrorToken(self.text)}; // Error Token handler
