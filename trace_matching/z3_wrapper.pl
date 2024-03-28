#!/usr/bin/env -S -- swipl -q --stack-limit=6G -g main -t halt

:- use_module(library(main)).
:- use_module(sexpr).
:- use_module(library(thread)).


state(S), [S] --> [S].
state(S0, S), [S] --> [S0].

split_smt([]) -->
    state(s(Out, [A|_]), s(Out1, [])),
    { maplist(reverse,[A | Out], Out0), reverse(Out0, Out1) }.
split_smt([[push]|Stmts]) --> !,
    state(s(Out,[A|As]), s(Out, [A|[A|As]])),
    split_smt(Stmts).
split_smt([[pop]|Stmts]) --> !,
    state(s(Out, [A | [B | Cs]]), s([A|Out], [B|Cs])),
    split_smt(Stmts).
split_smt([S|Stmts]) -->
    state(s(Out, [A | As]), s(Out, [[S|A] | As])),
    split_smt(Stmts).

split_smt(Smt, Out) :-
    phrase((split_smt(Smt), state(s(Out, _))), [s([],[[]])], _).


once_in_module(M, Goal, Arg) :- call(M:Goal, Arg), !.
ml_goal(Goal, Elem, call(Goal, Elem)).
workers(List, Count) :-
    current_prolog_flag(cpu_count, Cores),
    Cores > 1,
    length(List, Len),
    MaxCPU is min(6, Cores),
    Count is min(MaxCPU, min(Cores,Len)),
    Count > 1,
    !.

concurrent_maplist(Goal, List) :-
    workers(List, WorkerCount),
    !,
    maplist(ml_goal(Goal), List, Goals),
    concurrent(WorkerCount, Goals, []).
concurrent_maplist(M:Goal, List) :-
    maplist(once_in_module(M, Goal), List).


main([]) :- !, main(['/dev/stdin']).
main(['-t', Timeout]) :- !, main(['-t', Timeout, '/dev/stdin']).
main([Filename]) :- !, main(['-t', '600', Filename]).
main(['-t', Timeout, Filename]) :- !,
    read_file_to_string(Filename, S, []),
    string_chars(S, Chars),
    (phrase(sexprs(Smt), Chars) ; throw(error(parse_error,_))),
    split_smt(Smt, Smts),
    mutex_create(Mid),
    concurrent_maplist([Query]>>(
                with_mutex(Mid, tmp_file_stream(TmpName, TmpStream, [extension('smt2'), encoding('text')])),
                maplist(list_sexpr, Query, Qs),
                maplist(writeln(TmpStream), Qs),
                close(TmpStream),
                with_mutex(Mid, tmp_file_stream(OutName, OutStream, [extension('out'), encoding('text')])),
                format(string(TimeoutArg), "timeout=~a000", [Timeout]),
                catch(
                    process_create(path(z3), [TimeoutArg, "sat.random_seed=31337", "smt.random_seed=31337", TmpName], [stdout(stream(OutStream))]),
                    error(process_error(_, exit(_)), _),
                    true),
                close(OutStream),
                catch(
                    process_create(path(cat), [OutName], [stdout(std)]),
                    error(process_error(_, exit(_)), _),
                    true),
                delete_file(OutName)
    ), Smts).
