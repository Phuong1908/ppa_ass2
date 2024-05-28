% main program
go :- 	open('input.txt', read, Stream),
        read_term(Stream, Y, []),
        close(Stream),
        open('output.txt', write, Stream1),
        set_output(Stream1),
        catch(reduce_prog(Y), Exception, process(Exception)),
        close(Stream1).

reduce_prog([Var, Func, Body]) :-
    create_env(Var, env([], 0, 0), Env), % Init env from the the global env
    type_check_func(Env, Func, Env1), % Do type check in the function/procedure body
    type_check_body(Env1, Body), % Do type check in the program body
    create_runtime_env(Env1, REnv),
    reduce_stmt(config(Body, REnv), _).

% Error handling
process(type_mismatch(X)):- write('Type mismatch: '), write(X), !.
process(undeclare_identifier(X)):- write('Undeclared identifier: '), write(X), !.
process(wrong_number_of_argument(X)):- write('Wrong number of arguments: '), write(X), !.
process(redeclare_identifier(X)):- write('Redeclared identifier: '), write(X), !.
process(redeclare_function(X)):- write('Redeclared function: '), write(X), !.
process(redeclare_procedure(X)):- write('Redeclared procedure: '), write(X), !.
process(undeclare_function(X)):- write('Undeclared function: '), write(X), !.
process(undeclare_procedure(X)):- write('Undeclared procedure: '), write(X), !.
process(break_not_in_loop(X)):- write('Break not in a loop: '), write(X), !.
process(continue_not_in_loop(X)):- write('Continue not in a loop: '), write(X), !.
process(cannot_assign(X)) :- write('Cannot assign to a constant: '), write(X), !.
process(outofbound(X)):- write('Index out of bound: '), write(X), !.
process(invalid_expression(X)):- write('Invalid expression: '), write(X), !.

% Lookup in symbol table
lookup(env([], _, _), X, _) :- throw(undeclare_identifier(X)).
lookup(env([id(X, Y, Z) | _], _, _), X, id(X, Y, Z)) :- !.
lookup(env([_ | L], B, T), X, Y) :- lookup(env(L, B, T), X, Y).

% Check if X is an identifier
atom1(true) :- !, fail.
atom1(false) :- !, fail.
atom1([]) :- !, fail.
atom1(X) :- atom(X).

% Check if X is a boolean constant
boolean(true).
boolean(false).

% Get type of expression
get_type_expression(Env, Y, T) :- atom1(Y), !, lookup(Env, Y, id(Y, _, T)).
get_type_expression(_, Y, integer) :- integer(Y), !.
get_type_expression(_, Y, real) :- float(Y), !.
get_type_expression(_, str(_), string).
get_type_expression(_, Y, boolean) :- boolean(Y), !.
get_type_expression(_, sub(Y), integer) :- integer(Y), !.
get_type_expression(_, sub(Y), float) :- float(Y), !.

% Logical expressions
get_type_expression(_, bnot(_), boolean).
get_type_expression(_, band(_, _), boolean).
get_type_expression(_, bor(_, _), boolean).

% Numerical expressions
get_type_expression(Env, add(X, Y), integer) :- 
    get_type_expression(Env, X, integer),
    get_type_expression(Env, Y, integer).
get_type_expression(Env, add(X, Y), float) :- 
    (get_type_expression(Env, X, float);
    get_type_expression(Env, Y, float)).
get_type_expression(Env, sub(X, Y), integer) :- 
    get_type_expression(Env, X, integer),
    get_type_expression(Env, Y, integer).
get_type_expression(Env, sub(X, Y), float) :- 
    (get_type_expression(Env, X, float);
    get_type_expression(Env, Y, float)).
get_type_expression(Env, times(X, Y), integer) :- 
    get_type_expression(Env, X, integer),
    get_type_expression(Env, Y, integer).
get_type_expression(Env, times(X, Y), float) :- 
    (get_type_expression(Env, X, float);
    get_type_expression(Env, Y, float)).
get_type_expression(Env, rdiv(X, Y), integer) :- 
    get_type_expression(Env, X, integer),
    get_type_expression(Env, Y, integer).
get_type_expression(Env, rdiv(X, Y), float) :- 
    (get_type_expression(Env, X, float);
    get_type_expression(Env, Y, float)).

get_type_expression(Env, idiv(X, Y), integer) :- 
    get_type_expression(Env, X, integer),
    get_type_expression(Env, Y, integer).
get_type_expression(Env, imod(X, Y), integer) :- 
    get_type_expression(Env, X, integer),
    get_type_expression(Env, Y, integer).

% Relational expressions
get_type_expression(Env, greater(X, Y), boolean) :-
    get_type_expression(Env, X, _),
    get_type_expression(Env, Y, _).
get_type_expression(Env, less(X, Y), boolean) :-
    get_type_expression(Env, X, _),
    get_type_expression(Env, Y, _).
get_type_expression(Env, ge(X, Y), boolean) :-
    get_type_expression(Env, X, _),
    get_type_expression(Env, Y, _).
get_type_expression(Env, le(X, Y), boolean) :-
    get_type_expression(Env, X, _),
    get_type_expression(Env, Y, _).
get_type_expression(Env, ne(X, Y), boolean) :-
    get_type_expression(Env, X, _),
    get_type_expression(Env, Y, _).
get_type_expression(Env, eql(X, Y), boolean) :-
    get_type_expression(Env, X, _),
    get_type_expression(Env, Y, _).

% Element expressions

% Type check assignment
type_check_assignment(Env, integer, Y) :- get_type_expression(Env, Y, integer).
type_check_assignment(Env, real, Y) :- get_type_expression(Env, Y, real).
type_check_assignment(Env, string, Y) :- get_type_expression(Env, Y, string).
type_check_assignment(Env, boolean, Y) :- get_type_expression(Env, Y, boolean).

% Type checking one statement
type_check_stmt(Env, assign(X, Y)) :- 
    lookup(Env, X, id(X, _, T)),
    type_check_assignment(Env, T, Y).

% Type checking for a call statement
% Built-in functions
type_check_stmt(Env, call(Func, [X])) :- 
    (is_write_func(Func); is_write_ln_func(Func)),
    get_type_expression(Env, X, _).

% User-defined functions									
% Type check one block										
type_check_body(_, []).
type_check_body(env(L, B, T), [var(X, Y) | _]) :- 
    has_declared(X, env(L, B, T)), !,
    throw(redeclare_identifier(var(X, Y))).
type_check_body(env(L, B, T), [var(X, Y) | L1]) :- 
    T1 is T + 1, 
    type_check_body(env([id(X, var, Y) | L], B, T1), L1), !.

type_check_body(Env, [X | L]) :- 
    type_check_stmt(Env, X), 
    type_check_body(Env, L).

% Type checking a procedure
type_check_one_func(Env, proc(_, Y, Z)):- 
    create_env(Y, Env, env(L, _, T)),
	type_check_body(env(L, T, T), Z).

% Type checking a list of procedures
type_check_func(Env, [], Env).
type_check_func(_, [proc(X, _, _) | _], _) :- 
    is_builtin(X), !,
    throw(redeclare_procedure(X)).
type_check_func(Env, [proc(X, _, _) | _], _) :- 
    has_declared(X, Env), !,
    throw(redeclare_procedure(X)).
type_check_func(env(Env, B, T), [proc(X, Y, Z) | L], Env1) :- 
    T1 is T + 1,
    type_check_one_func(env([id(X, proc, proc(Y, Z)) | Env], T1, T1), proc(X, Y, Z)), !,
    type_check_func(env([id(X, proc, proc(Y, Z)) | Env], B, T1), L, Env1).

% Check if X has been declared in the symbol table from B+1 to T
has_declared(X, env([id(X, _, _) | _], B, T)):- 
    T > B, !.
has_declared(X, env([_ | L], B, T)) :- 
    T1 is T - 1, 
    has_declared(X, env(L, B, T1)).

% Create a symbol table from the list of variable or constant declarations
create_env([], L, L).
create_env([var(X, Y) | _], env(_, 0, _), _) :- 
    is_builtin(X), !,
    throw(redeclare_identifier(var(X, Y))).
create_env([var(X, Y) | _], L1, _) :- 
    has_declared(X, L1), !,
    throw(redeclare_identifier(var(X, Y))).
create_env([var(X, Y) | L], env(L1, B, T), L2) :- 
    T1 is T + 1, 
    create_env(L, env([id(X, var, Y) | L1], B, T1), L2).

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

% Check if the function is a write function
is_write_func(writeInt).
is_write_func(writeReal).
is_write_func(writeBool).
is_write_func(writeStr).

% Check if the function is a writeLn function
is_write_ln_func(writeIntLn).
is_write_ln_func(writeRealLn).
is_write_ln_func(writeBoolLn).
is_write_ln_func(writeStrLn).

% For runtime
create_runtime_env(X, X).

% Reduce expressions
reduce(config(add(E1, E2), Env), config(R, Env)) :-
    reduce_all(config(E1, Env), config(V1, Env)),
    reduce_all(config(E2, Env), config(V2, Env)),
    R is V1 + V2.

reduce(config(sub(E1), Env), config(R, Env)) :-
    reduce_all(config(E1, Env), config(V1, Env)),
    R is -V1.

reduce(config(sub(E1, E2), Env), config(R, Env)) :-
    reduce_all(config(E1, Env), config(V1, Env)),
    reduce_all(config(E2, Env), config(V2, Env)),
    R is V1 - V2.

reduce(config(times(E1, E2), Env), config(R, Env)) :-
    reduce_all(config(E1, Env), config(V1, Env)),
    reduce_all(config(E2, Env), config(V2, Env)),
    R is V1 * V2.

reduce(config(rdiv(E1, E2), Env), config(R, Env)) :-
    reduce_all(config(E1, Env), config(V1, Env)),
    reduce_all(config(E2, Env), config(V2, Env)),
    R is V1 / V2.

reduce(config(idiv(E1, E2), Env), config(R, Env)) :-
    reduce_all(config(E1, Env), config(V1, Env)),
    reduce_all(config(E2, Env), config(V2, Env)),
    R is V1 // V2.

reduce(config(imod(E1, E2), Env), config(R, Env)) :-
    reduce_all(config(E1, Env), config(V1, Env)),
    reduce_all(config(E2, Env), config(V2, Env)),
    R is mod(V1, V2).

% Reduce all expressions in the list until there is no expression to reduce
reduce_all(config(V, Env), config(V, Env)) :- number(V), !.
reduce_all(config(V, Env), config(V, Env)) :- boolean(V), !.
reduce_all(config(str(V), Env), config(V, Env)) :- !.
reduce_all(config(E, Env), config(E2, Env)) :-
    reduce(config(E, Env), config(E1, Env)), !,
    reduce_all(config(E1, Env), config(E2, Env)).

% Reduce a list of expressions
reduce_all(config([], Env), config([], Env)).
reduce_all(config([S | L], Env), config([S1 | L1], Env1)) :-
    reduce_all(config(S, Env), config(S1, Env)),
    reduce_all(config(L, Env), config(L1, Env1)).

% Reduce a list of statements
reduce_stmt(config([], Env), Env).
reduce_stmt(config([Stmt | Stmts], Env), Env1) :-
    reduce_stmt(config(Stmt, Env), Env2),
    reduce_stmt(config(Stmts, Env2), Env1).

% Handle call(writeInt, [X]) statement
% Handle call statements, including built-in and user-defined functions
reduce_stmt(config(call(Func, Args), Env), Env) :-
    (is_builtin(Func) -> 
        handle_builtin(Func, Args, Env); 
        handle_user_defined(Func, Args, Env)).

% Handle built-in functions generically
handle_builtin(Func, [X], Env) :-
    reduce_all(config(X, Env), config(V, Env)),
    (is_write_func(Func) -> write(V) ; true),
    (is_write_ln_func(Func) -> writeln(V) ; true),
    flush_output.

% Handle user-defined functions
handle_user_defined(Func, Args, Env) :-
    lookup(Env, Func, id(Func, proc, proc(Params, Body))),
    create_env(Params, env([], 0, 0), LocalEnv),
    bind_args(Params, Args, LocalEnv, Env1),
    reduce_stmt(config(Body, env(Env1, 0, 0)), _).

% Bind arguments to parameters in the local environment
bind_args([], [], Env, Env).
bind_args([param(P, _) | Params], [Arg | Args], LocalEnv, Env) :-
    reduce_all(config(Arg, Env), config(Value, Env)),
    create_env([var(P, Value)], LocalEnv, Env1),
    bind_args(Params, Args, Env1, Env).
