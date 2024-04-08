%main program
go :- 	open('input.txt',read,Stream),
		read_term(Stream,Y,[]),
		close(Stream),
		open('output.txt',write,Stream1),
		set_output(Stream1),
		catch(reduce_prog(Y),Exception,process(Exception)),
        close(Stream1).
% The first three predicates are used to do type checking while the last two do executing the input program
reduce_prog([Var,Func,Body]) :-
		create_env(Var,env([],0,0),Env), % Init env from the the global env
		type_check_func(Env,Func,Env1), % Do type check in the function/procedure body
		type_check_body(Env1,Body), % Do type check in the program body
		create_runtime_env(Env1,REnv),
		reduce_stmt(config(Body,REnv),_). 

% For type checking

% Error handling

% Type checking

process(type_mismatch(X)):- write('Type mismatch: '),write(X),!. 
process(undeclare_identifier(X)):- write('Undeclared identifier: '),write(X),!. 
process(wrong_number_of_argument(X)):- write('Wrong number of arguments: '),write(X),!.
process(redeclare_identifier(X)):- write('Redeclared identifier: '),write(X),!. 
process(redeclare_function(X)):- write('Redeclared function: '),write(X),!. 
process(redeclare_procedure(X)):- write('Redeclared procedure: '),write(X),!. 
process(undeclare_function(X)):- write('Undeclared function: '),write(X),!. 
process(undeclare_procedure(X)):- write('Undeclared procedure: '),write(X),!. 
process(break_not_in_loop(X)):- write('Break not in a loop: '),write(X),!. 
process(continue_not_in_loop(X)):- write('Continue not in a loop: '),write(X),!. 
process(cannot_assign(X)) :- write('Cannot assign to a constant: '),write(X),!.

% Runtime error
process(outofbound(X)):- write('Index out of bound: '),write(X),!.
process(invalid_expression(X)):- write('Invalid expression: '),write(X),!.		

%lookup(symbol table,id,entry)
lookup(env([],_,_),X,_):- throw(undeclare_identifier(X)).
lookup(env([id(X,Y,Z)|_],_,_),X,id(X,Y,Z)):-!.
lookup(env([_|L],B,T),X,Y):- lookup(env(L,B,T),X,Y).

%atom1 check X is an identifier in the input program
atom1(true) :- !,fail.
atom1(false) :- !,fail.
atom1([]) :- !,fail.
atom1(X) :- atom(X).

%check X is a boolean constant
boolean(true).
boolean(false).


%get type of expression Y based on the symbol table Env. T is the return type
% TODO
get_type_expression(Env,Y,T) :- atom1(Y),!,lookup(Env,Y,id(Y,_,T)).
get_type_expression(_,Y,integer) :- integer(Y),!.
get_type_expression(_,Y,real) :- float(Y),!.
get_type_expression(_,str(_),string).
get_type_expression(_,Y,boolean) :- boolean(Y),!.
get_type_expression(_,sub(Y),integer) :- integer(Y),!.
get_type_expression(_,sub(Y),float) :- float(Y),!.

% Logical expresstion
get_type_expression(_,bnot(_),boolean).
get_type_expression(_,band(_,_),boolean).
get_type_expression(_,bor(_,_),boolean).
% Let it work first. Will refactor later :)))

% Numerical expression
get_type_expression(_,add(X,Y),integer) :- integer(X),integer(Y).
get_type_expression(_,add(X,Y),float) :- float(X);float(Y).
get_type_expression(_,sub(X,Y),integer) :- integer(X),integer(Y).
get_type_expression(_,sub(X,Y),float) :- float(X);float(Y).
get_type_expression(_,times(X,Y),integer) :- integer(X),integer(Y).
get_type_expression(_,times(X,Y),float) :- float(X);float(Y).
get_type_expression(_,rdiv(X,Y),integer) :- integer(X),integer(Y).
get_type_expression(_,rdiv(X,Y),float) :- float(X);float(Y).

get_type_expression(_,idiv(X,Y),integer) :- integer(X),integer(Y).
get_type_expression(_,imod(X,Y),integer) :- integer(X),integer(Y).

% Relational expression
get_type_expression(_,greater(_),boolean).
get_type_expression(_,less(_,_),boolean).
get_type_expression(_,ge(_,_),boolean).
get_type_expression(_,le(_),boolean).
get_type_expression(_,ne(_,_),boolean).
get_type_expression(_,eql(_,_),boolean).

% TODO: Index expression


% TODO
type_check_assignment(_,_,_).
type_check_assignment(_, integer, Y) :- get_type_expression(_, Y, integer).
type_check_assignment(_, real, Y) :- get_type_expression(_, Y, real).
type_check_assignment(_, string, Y) :- get_type_expression(_, Y, string).
type_check_assignment(_, boolean, Y) :- get_type_expression(_, Y, boolean).

% type checking one statement
type_check_stmt(Env,assign(X,Y)) :- lookup(Env,X,T),type_check_assignment(Env,T,Y).

% type checking for a call statement
% buit-in functions
type_check_stmt(Env,call(writeInt,[X])) :- get_type_expression(Env,X,integer).
type_check_stmt(Env,call(writeIntLn,[X])) :- get_type_expression(Env,X,integer).
type_check_stmt(Env,call(writeReal,[X])) :- get_type_expression(Env,X,float).
type_check_stmt(Env,call(writeRealLn,[X])) :- get_type_expression(Env,X,float).
type_check_stmt(Env,call(writeBool,[X])) :- get_type_expression(Env,X,boolean).
type_check_stmt(Env,call(writeBoolLn,[X])) :- get_type_expression(Env,X,boolean).
type_check_stmt(Env,call(writeStrLn,[X])) :- get_type_expression(Env,X,string).
type_check_stmt(Env,call(writeStrLn,[X])) :- get_type_expression(Env,X,string).

% user-defined functions
									
%type check one block										
type_check_body(_,[]).
type_check_body(env(L,B,T),[var(X,Y)|_]) :- has_declared(X,env(L,B,T)),!,throw(redeclare_identifier(var(X,Y))).
type_check_body(env(L,B,T),[var(X,Y)|L1]) :- T1 is T + 1, type_check_body(env([id(X,var,Y)|L],B,T1),L1),!.

type_check_body(Env,[X|L]) :- type_check_stmt(Env,X), type_check_body(Env,L).

%type checking a procedure
type_check_one_func(Env,proc(_,Y,Z)):- create_env(Y,Env,env(L,_,T)),
										type_check_body(env(L,T,T),Z).
									
%type checking a list of procedures
type_check_func(Env,[],Env).
type_check_func(_,[proc(X,_,_)|_],_) :- is_builtin(X),!,throw(redeclare_procedure(X)).
type_check_func(Env,[proc(X,_,_)|_],_) :- has_declared(X,Env),!,throw(redeclare_procedure(X)).
type_check_func(env(Env,B,T),[proc(X,Y,Z)|L],Env1) :- T1 is T+1,
											type_check_one_func(env([id(X,proc,proc(Y,Z))|Env],T1,T1),proc(X,Y,Z)), !,
											type_check_func(env([id(X,proc,proc(Y,Z))|Env],B,T1),L,Env1).
											
%check if X has been declared in the symbol table from B+1 to T
has_declared(X,env([id(X,_,_)|_],B,T)):- T > B ,!.
has_declared(X,env([_|L],B,T)) :- T1 is T - 1, has_declared(X,env(L,B,T1)).

% create a symbol table from the list of variable or constant declarations
% As a suggestion, we may implement a symbol table as the functor env(list,bottom,top)
% where list contains the list of entries id(identifier,kind,type), bottom
% is the index of the first element in the current scope minus 1, and top is the
% index of the last element in the current scope. 
% We have the right to implement the symbol table in a different way.
create_env([],L,L).
create_env([var(X,Y)|_],env(_,0,_),_):- is_builtin(X),!,throw(redeclare_identifier(var(X,Y))).
create_env([var(X,Y)|_],L1,_):-has_declared(X,L1),!,throw(redeclare_identifier(var(X,Y))).
create_env([var(X,Y)|L],env(L1,B,T),L2):- T1 is T+1, create_env(L,env([id(X,var,Y)|L1],B,T1),L2).

is_builtin(readInt).
is_builtin(writeIntLn).
is_builtin(writeInt).
is_builtin(readReal).
is_builtin(writeRealLn).
is_builtin(writeReal).
is_builtin(readBool).
is_builtin(writeBoolLn).
is_builtin(writeBool).
is_builtin(writeLn).
is_builtin(writeStrLn).
is_builtin(writeStr).

% For runtime
%TODO
create_runtime_env(X,X).

reduce(config(sub(E1),Env),config(R,Env)) :-  
        reduce_all(config(E1,Env),config(V1,Env)),
        R is -V1.

reduce(config(add(E1,E2),Env),config(R,Env)) :-  
		reduce_all(config(E1,Env),config(V1,Env)),
		reduce_all(config(E2,Env),config(V2,Env)),
		R is V1+V2.

reduce(config(sub(E1,E2),Env),config(R,Env)) :-  
        reduce_all(config(E1,Env),config(V1,Env)),
        reduce_all(config(E2,Env),config(V2,Env)),
        R is V1-V2.

reduce(config(times(E1,E2),Env),config(R,Env)) :-  
        reduce_all(config(E1,Env),config(V1,Env)),
        reduce_all(config(E2,Env),config(V2,Env)),
        R is V1*V2.

reduce(config(rdiv(E1,E2),Env),config(R,Env)) :-    % missing test case
        reduce_all(config(E1,Env),config(V1,Env)),
        reduce_all(config(E2,Env),config(V2,Env)),
        R is V1/V2.

reduce(config(idiv(E1,E2),Env),config(R,Env)) :-  
        reduce_all(config(E1,Env),config(V1,Env)),
        reduce_all(config(E2,Env),config(V2,Env)),
        R is V1 // V2.

reduce(config(imod(E1,E2),Env),config(R,Env)) :-  
        reduce_all(config(E1,Env),config(V1,Env)),
        reduce_all(config(E2,Env),config(V2,Env)),
        R is mod(V1,V2).

reduce_all(config(V,Env),config(V,Env)):- integer(V),!.

reduce_all(config(E,Env),config(E2,Env)):-
		reduce(config(E,Env),config(E1,Env)),!,
		reduce_all(config(E1,Env),config(E2,Env)).

reduce_stmt(config([call(writeInt,[X])],_),_) :- 
		reduce_all(config(X,Env),config(V,Env)),
		write(V)
        .
