#!/usr/bin/env -S -- swipl -q  --stack-limit=4G -g main -t halt

:- style_check(-singleton).

:- use_module(library(main)).
:- use_module(library(pcre)).
:- use_module(library(clpfd)).
:- use_module(sexpr).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Utils

maplist_append(Goal, L0, L) :-
    maplist(Goal, L0, L1),
    append(L1, L).

% streams
seq_car(seq(Car,_), Car).
seq_next(seq(_, Cdr0), Seq) :-
    call(Cdr0, Seq).

seq_take(0, _, []) :- !.
seq_take(N, Seq, [E|Ls]) :-
    M is N - 1,
    seq_car(Seq, E),
    seq_next(Seq, Next),
    seq_take(M, Next, Ls).

repeat(E, seq(E, repeat(E))).

seq_of_list([], nil).
seq_of_list([X|Xs], seq(X, seq_of_list(Xs))).

% smtlib visitor
:- op(300, xfy, =>).
L => B :-
    \+ is_list(L), !,
    [L] => B.
Ls => B :-
    is_list(Ls), !,
    setup_call_cleanup(
        maplist(asserta, Ls, Refs),
        B,
        maplist(erase, Refs)).

:- dynamic copy_smtlib/2.
copy_smtlib(X,Y) :- is_list(X), !, maplist(copy_smtlib, X, Y).
copy_smtlib(X, X).

:- dynamic foldmap_smtlib/4.
foldmap_smtlib([], A, [], A) :- !.
foldmap_smtlib([H|T],A,[H1|T1],B) :- !, foldmap_smtlib(H, A, H1, A1), foldmap_smtlib(T, A1, T1, B).
foldmap_smtlib(X, A, X, A).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Rewriting

desugar_define_datatype(E, O) :-
    (  E = ['declare-datatype', Name | Defs] -> O = [['declare-datatypes', [[Name, 0]], Defs]]
    ; E = ['declare-datatypes', Ns, Defs] -> transpose([Ns, Defs], L), maplist([[N | D], ['declare-datatypes', [N], D]]>>true, L, O)
    ; O = [E]
    ).
desugar_declare_const(E, O) :-
    ( E = ['declare-const', Name, Sort] -> O = [['declare-fun', Name, [], Sort]]
    ; O = [E]
    ).

desugar_assert_not(E, O) :-
    ( E = ['assert-not', Body] -> O = [['assert', ['not', Body]]]
    ; O = [E]
    ).

define_list_type(E,[['declare-datatypes', [['List', 1]], [[par, [x], [[insert, [head, x], [tail, ['List', x]]], [nil]]]]] | E]).

remove_big_strings(Length,Smtlib0, Smtlib) :-
    (copy_smtlib(S, S1) :-
         string(S),
         string_length(S, N), N > Length,
         sub_string(S, 0, Length, _, S1), !)
    => copy_smtlib(Smtlib0, Smtlib).

string_compat(Smtlib0, Smtlib) :-
    [
        (copy_smtlib('str.to.re', 'str.to_re') :- !),
        (copy_smtlib('str.in.re', 'str.in_re') :- !)
    ] => copy_smtlib(Smtlib0, Smtlib).

string_non_print_escape(Smtlib0, Smtlib) :-
    (copy_smtlib(S, S1) :-
         string(S), !,
         escape_non_print(S, S1), !) => copy_smtlib(Smtlib0, Smtlib).


datatype_projections(['declare-datatypes', [[_, Arity]], [Definition0]], Cs) :-
    findall(C, datatype_projections_(Arity, Definition0, C), Cs).
datatype_projections_(Arity, Definition0, CName-Names) :-
    ( Arity = 0 ->
      Definition = Definition0
    ; Definition0 = [par, _, Definition]
    ),
    member([CName | Args], Definition),
    maplist([[PrName, _], PrName]>>true, Args, Names).

bind_vars(Ds, Constr, Vars, Elem, Expr0, Expr) :-
    member(D, Ds),
    datatype_projections(D, Ps),
    member(Constr-Names, Ps), !,
    (length(Vars, N), length(Names, N) ; throw(error(invalid_match_pattern(Constr-Vars-Elem),_))), !,
    maplist({Elem}/[Var,Name,Bind]>>(Bind = [Var, [Name, Elem]]), Vars, Names, Binds),
    Expr = [let, Binds, Expr0].

desugar_match1(Datatypes, ['match', Value, Cases], Ds) :-
    maplist({Datatypes, Value}/[[Patt, Res], Cond-Body]>>(
                is_list(Patt) -> Patt = [Name | Vars], Cond = [['_', is, Name], Value], bind_vars(Datatypes, Name, Vars, Value, Res, Body)
                ; Cond = [['_', is, Patt], Value], Body = Res
            ), Cases, Branches),
    Branches = [ Last | Rest ],
    foldl([_-L,C-L1,[ite, C, L1, L]]>>true, Rest, Last, Ds).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Type checking

replace(_, _, [], []).
replace(Old, New, [Old|L], [New|NL]) :-
    replace(Old, New, L, NL).
replace(Old, New, [A|L], [B|NL]) :-
    A \= Old,
    ( is_list(A) -> replace(Old, New, A, B)
    ; A = B),
    replace(Old, New, L, NL).


fn_sort(['declare-fun', Name, Args, Ret], Name-Args-Ret).
fn_sort([Fn, Name, Args, Ret, _Body], Name-ArgTs-Ret) :-
    member(Fn, ['define-fun', 'define-fun-rec']),
    maplist(nth0(1), Args, ArgTs).
% constructors
fn_sort(['declare-datatypes', [[Name, Arity]], [Definition0]], FName-FArgs-Ret) :-
    ( Arity = 0 ->
      ( Ret = Name,
        Definition = Definition0,
        member([FName | Args], Definition),
        maplist(nth0(1), Args, FArgs)
      )
    ; length(Ls, Arity),
      Definition0 = [par, Vs, Definition],
      member([FName | Args], Definition),
      maplist(nth0(1), Args, FArgs0),
      foldl([V,L,A,O]>>replace(V, L, A, O), Vs, Ls, FArgs0, FArgs),
      Ret = [Name | Ls] ).
% projections
fn_sort(['declare-datatypes', [[Name, Arity]], [Definition0]], PrName-[Ret]-PrArg) :-
    ( Arity = 0 ->
      Definition = Definition0,
      Ret = Name
    ; length(Ls, Arity),
      Definition0 = [par, Vs, Definition],
      Ret = [Name | Ls]
    ),
    member([_FName | Args], Definition),
    member([PrName, PrArg0], Args),
    ( Arity = 0 -> PrArg = PrArg0
    ; ((copy_smtlib(V, _) :- member(V, Vs), !)) => copy_smtlib(PrArg0, PrArg)
    ).

% get the signature of every user-defined predicate (including datatypes contructors/projections)
user_defined_sorts(Smtlib, Types) :- % PRE: requires desugar_define_datatype, desugar_define_const
    convlist([E,Vs]>>(findall(V, fn_sort(E,V), Vs)), Smtlib, Types0),
    append(Types0, Types).

% substitute polymorphic constant with (as <constant> <unconstrained variable>)
subst_polytypes(Ts, Smtlib0, Smtlib) :-
    [
        (copy_smtlib(['declare-datatypes' | D], ['declare-datatypes' | D]) :- !),
        (copy_smtlib(['match', E, Cases], ['match', E1, Cases1]) :- !,
             copy_smtlib(E, E1),
             maplist([[C,R], [C,R1]]>>copy_smtlib(R,R1), Cases, Cases1))
    ] => subst_polytypes_(Ts, Smtlib0, Smtlib).
subst_polytypes_([], Smtlib0, Smtlib) :- copy_smtlib(Smtlib0, Smtlib).
subst_polytypes_([Name-Args-Ret | Ts], Smtlib0, Smtlib) :-
    term_variables(Ret, Vs), Vs = [_|_], !,
    length(Args, N), length(NewVars, N),
    ([
        (copy_smtlib([Name | NewVars], [as, [Name | NewVars1], Ret]) :- !, copy_smtlib(NewVars, NewVars1)),
        (copy_smtlib(Name, [as, Name, Ret]) :- N = 0, !)
    ] => subst_polytypes_(Ts, Smtlib0, Smtlib)).
subst_polytypes_([_ | Ts ], Smtlib0, Smtlib) :-
    subst_polytypes_(Ts, Smtlib0, Smtlib).


branch_type(Ts, T, [N, B]) :-
    atom(N),
    of_type(Ts, B, T).
branch_type(Ts, T, [[Fn|Args],B]) :-
    member(Fn-TArgs-_, Ts),
    maplist([A,TA,R]>>(R = (of_type(_, A, TA) :- !)), Args, TArgs, Assumptions),
    Assumptions => of_type(Ts, B, T).

:- dynamic of_type/3.
% explicit types
of_type(Ts, [as, B, Type], Type) :- !, of_type(Ts, B, Type).
% quantifiers
of_type(Ts, [Quantifier, Vars, Body], Type) :-
    member(Quantifier, ['forall', 'exists']), !,
    Type = 'Bool',
    maplist([[VName, VType], A]>>(
                A = (of_type(_, VName, VType) :- !)
            ), Vars, Assumptions),
    (Assumptions => of_type(Ts, Body, Type)).
% match
of_type(Ts, ['match', _Elm, Cases], Type) :- !,
    %gtrace,
    %maplist({Type}/[[_,B]]>>of_type(Ts, B, Type), Cases).
    maplist(branch_type(Ts, Type), Cases); throw(error(type_error(match, Cases, Type, Ts))).
% let
of_type(Ts, ['let', Binds, Body], Type) :- !,
    maplist([[Nm,Vl],A]>>(
                of_type(Ts, Vl, Tp),
                A = (of_type(_, Nm, Tp) :- !)
            ), Binds, Assumptions),
    (Assumptions => of_type(Ts, Body, Type)).
% builtin constants
of_type(_, true, 'Bool') :- !.
of_type(_, false, 'Bool') :- !.
of_type(_, N, 'Int') :- number(N), !.
of_type(_, S, 'String') :- string(S), !.
of_type(_, 're.allchar', 'Regex') :- !.
% user defined constant
of_type(Ts0, Fn, Type) :-
    copy_term(Ts0, Ts),
    member(Fn-[]-TRet, Ts) -> Type = TRet.
% functions
of_type(Ts, [Fn | Args], Type) :-
    % builtin functions
    ( Fn = '=' ->
      maplist({Ts,T}/[E]>>of_type(Ts,E,T), Args),
      Type = 'Bool'
    ; member(Fn, ['>', '<','>=', '<=']) ->
      maplist({Ts}/[E]>>of_type(Ts,E,'Int'), Args),
      Type = 'Bool'
    ; member(Fn, ['+', '-', '*', '/']) ->
      maplist({Ts}/[E]>>of_type(Ts,E,'Int'), Args),
      Type = 'Int'
    ; Fn = ['_', 'is', _] ->
      Type = 'Bool'
    ; member(Fn, ['and', 'or', 'not', '=>']) ->
      maplist({Ts}/[E]>>of_type(Ts,E,'Bool'), Args),
      Type = 'Bool'
    ; Fn = 'ite' ->
      Args = [C, Tn, Es],
      of_type(Ts, C, 'Bool'), of_type(Ts, Tn, Type), of_type(Ts, Es, Type)
    ; member(Fn, ['str.++']) ->
      maplist({Ts}/[E]>>of_type(Ts,E,'String'), Args),
      Type = 'String'
    ; Fn = 'str.indexof' ->
      Args = [A, B, C], of_type(Ts, A, 'String'), of_type(Ts, B, 'String'), of_type(Ts, C, 'Int'),
      Type = 'Int'
    ; Fn = 'str.len' ->
      Args = [A], of_type(Ts, A, 'String'),
      Type = 'Int'
    ; member(Fn, ['str.contains', 'str.prefixof']) ->
      maplist({Ts}/[E]>>of_type(Ts,E,'String'), Args),
      Type = 'Bool'
    ; Fn = 'str.at' ->
      Args = [A, B], of_type(Ts, A, 'String'), of_type(Ts, B, 'Int'),
      Type = 'String'
    ; Fn = 'str.substr' ->
      Args = [A, B, C], of_type(Ts, A, 'String'), of_type(Ts, B, 'Int'), of_type(Ts, C, 'Int'),
      Type = 'String'
    ; member(Fn, ['str.in.re', 'str.in_re']) ->
      copy_term(Ts, Ts1),
      Args = [S,R], of_type(Ts, S, 'String'), of_type(Ts1, R, 'Regex'),
      Type = 'Bool'
    ; member(Fn, ['str.to.re', 'str.to_re', 're.range']) ->
      maplist({Ts}/[E]>>of_type(Ts,E,'String'), Args),
      Type = 'Regex'
    ; member(Fn, ['re.+', 're.++', 're.union', 're.*', 're.opt', ['_','re.loop',_,_] ]) ->
      maplist({Ts}/[E]>>of_type(Ts,E,'Regex'), Args),
      Type = 'Regex'
    % user defined functions
    ; (copy_term(Ts, Ts1), member(Fn-TArgs-TRet, Ts1)) ->
      maplist({Ts}/[A,T]>>(copy_term(Ts, Ts1), of_type(Ts1, A, T)), Args, TArgs),
      Type = TRet
    ), !.
% raise an exception on type error
of_type(Ts, E, T) :-
    throw(error(type_error(E, T, Ts))).

% ignore
check_type(_, [N | _]) :-
    member(N, ['declare-datatypes', 'declare-fun', 'push', 'pop', 'check-sat', 'get-model', 'get-proof', 'echo', 'set-option']), !.
% function definitions
check_type(Ts, [N, Fn, Params, Ret, Body]) :-
    member(N, ['define-fun', 'define-fun-rec']), !,
    maplist([[PName, PType],A]>>(
                A = (of_type(_, PName, PType) :- !)
            ), Params, Assumptions),
    maplist([[_,T],T]>>true, Params, PTypes),
    (Assumptions => of_type([Fn-PTypes-Ret|Ts], Body, Ret); throw(error(check_type_error(Body, Ret, Ts)))), !.
% assertions
check_type(Ts, [N, Body]) :-
    member(N, ['assert', 'assert-not']), !,
    (of_type(Ts, Body, 'Bool') ; throw(error(check_type_error(Body, 'Bool', Ts)))), !.

check_type(_, A) :-
    throw(error(check_type_error(invalid-smtlib, A))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Recipes

desugar(Smtlib0, Smtlib) :-
    maplist_append(desugar_define_datatype, Smtlib0, Smtlib1),
    maplist_append(desugar_declare_const, Smtlib1, Smtlib2),
    maplist_append(desugar_assert_not,Smtlib2, Smtlib).

cvc5(Smtlib0, Smtlib) :-
    define_list_type(Smtlib0, Smtlib1),
    desugar(Smtlib1, Smtlib2),
    % smtlib string compatibility
    string_non_print_escape(Smtlib2, Smtlib3),
    string_compat(Smtlib3, Smtlib4),
    % annotate every polymorphic constant or function
    user_defined_sorts(Smtlib4, Types),
    subst_polytypes(Types, Smtlib4, Smtlib5), !,
    maplist(check_type(Types), Smtlib5), !,
    % remove annotations on polymorphic functions
    (copy_smtlib([as, S, _], S1) :- is_list(S), !, copy_smtlib(S, S1))
    => copy_smtlib(Smtlib5, Smtlib).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Main

main(_) :-
    read_string(user, _, Input),
    string_chars(Input, Chars),
    (phrase(sexprs(Smtlib0), Chars) ; throw(error(parse_error,_))), !,

    cvc5(Smtlib0, Smtlib1),
    remove_big_strings(120,Smtlib1, Smtlib),

    (maplist([S,O]>>list_sexpr(S, O), Smtlib, Output) ; throw(error(sexpr_err,_))),
    maplist(writeln, Output),
    true.


% ./smtlib_compat.pl |  cvc5 --lang smtlib2 --incremental --strings-exp --produce-models --produce-proofs --
