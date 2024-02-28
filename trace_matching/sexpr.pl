#!/usr/bin/env false

:- module(sexpr, [list_sexpr/2, sexpr//1, sexprs//1, escape_non_print/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Parser

comment --> [X], { X \= '\n' }, !, comment.
comment --> [].
ws --> [C], { char_type(C, space) }, !.
ws --> [';'], comment, ['\n'], !.
wss --> ws, !, wss.
wss --> [].
lpar --> ['('], wss.
rpar --> [')'], wss.

sym_ch([C|Cs]) --> [C], { char_type(C, print), \+ char_type(C, space), \+ member(C, ['(',')', '\'', '"']) }, !, sym_ch(Cs).
sym_ch([]) --> [].

digits([X|Xs]) --> [X], { (char_type(X, digit) ; member(X, '.')) }, !, digits(Xs).
digits([]) --> [].

quoted([C|Cs]) --> [C], { char_type(C, print), C \= '"' }, !, quoted(Cs).
quoted([X|Xs]) --> [X], {member(X, [' ','\n','\r','\t'])}, !, quoted(Xs).
quoted(['"'|Xs]) --> ['\\','"'], !, quoted(Xs).
quoted(['"'|Xs]) --> ['"','"'], !, quoted(Xs).
quoted(['\\'|Xs]) --> ['\\','\\'], !, quoted(Xs).
quoted([]) --> [].

num(A) --> digits(C), { C \= [], number_chars(A,C) }, wss.
sym(A) --> sym_ch(C), { C \= [], atom_chars(A,C) }, wss.
str(A) --> ['"'], quoted(X), ['"'], { string_chars(A,X) }, wss.

sexpr(X) --> wss, sexpr_(X).
sexpr_(X) --> num(X), !.
sexpr_(X) --> sym(X), !.
sexpr_(X) --> str(X), !.
sexpr_(X) --> lpar, sexprs(X), wss, rpar.
sexprs([X|Xs]) --> sexpr(X), !, sexprs(Xs).
sexprs([]) --> [].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Show

sepbyspace([X]) --> show_sexpr(X), !.
sepbyspace([X|Xs]) --> show_sexpr(X), ` `, sepbyspace(Xs), !.
show_sexpr([]) --> `()`, !.
show_sexpr(X) --> { is_list(X) }, `(`, sepbyspace(X), `)`, !.
show_sexpr(X) --> { atom(X), atom_string(X, Xs), string_codes(Xs, Cs) }, Cs, !.
show_sexpr(X) --> { \+ is_list(X), \+ atom(X), term_string(X, Xs0), escape_non_print(Xs0, Xs), string_codes(Xs, Cs0), escape_u(Cs0, Cs) }, Cs, !.

escape_u([], []).
escape_u([92,92,117|T], [92,117|E]) :- escape_u(T,E), !.
escape_u([H|T], [H|E]) :- escape_u(T,E).

escape_non_print(String0, String) :-
    re_foldl([Match, V0, V]>>(
                 get_dict(0, Match, Zero),
                 format(string(With), "\\u~|~`0t~16r~4+", Zero),
                 split_string(V0, Zero, "", V1),
                 atomics_to_string(V1, With, V)
             ), "[[:^print:]]", String0, String0, String, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Entrypoint

read_sexpr(Str,Sexpr) :- string_chars(Str,Cs),  phrase(sexpr(Sexpr), Cs).
show_sexpr(T,S) :- phrase(show_sexpr(T), C), string_codes(S,C).

list_sexpr(List, String) :-
    ( var(List) -> read_sexpr(String, List)
    ; var(String) -> show_sexpr(List, String)
    ; false ).
