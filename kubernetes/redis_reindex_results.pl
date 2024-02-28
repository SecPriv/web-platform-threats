#!/home/sp/.nix-profile/bin/env -S -- swipl -q  --stack-limit=4G -g main -t halt

:- use_module(library(main)).
:- use_module(library(pcre)).
:- use_module(library(clpfd)).
:- use_module(library(redis)).
:- use_module(library(thread)).

trim(L0,L) :- pcre:re_replace("^\s+|\s+$"/g, "", L0, L).

get_answers(Contents, Ans) :-
    re_split("\n", Contents, Lines),
    reverse(Lines, RLines),
    foldl([L0,A0,A]>>(
              trim(L0, L),
              ( (L = "unsat" ; L = "sat"; L = "unknown") ->
                A = [[L]|A0]
              ; re_matchsub("=+ (\\w+) =+", L, Sub, []), A0 = [Ls|A1], Ls = [_,_] ->
                get_dict(1, Sub, S1),
                A = [[S1 | Ls]|A1]
              ; re_match("^[^=\n(]", L), A0 = [Ls|A1], Ls = [_] ->
                A = [[L|Ls]|A1]
              ; A = A0)), RLines, [], Ans).


filter_by_timestamps(Start, End, K, K) :-
    re_matchsub("results:([^:]+):([^:]+):([^:]+):z3", K, Sub, []),
    get_dict(3, Sub, KTime),
    number_string(N, KTime),
    N >= Start,
    N =< End.

redis_invariants(Server, Options, Start, End) :-
    redis_server(rd, Server, Options),
    redis(rd, keys('results:*:z3'), Ks), 
    convlist(filter_by_timestamps(Start, End), Ks, Ks1),
    length(Ks1, N),
    format("total: ~w\n", [N]),
    concurrent_maplist({Start,End,N}/[K]>>(
              format("~w\n", [K]),
              re_matchsub("results:([^:]+):.*", K, Sub, []), get_dict(1, Sub, BR),
              redis(rd, get(K), Ct),
              get_answers(Ct, Ans),
              maplist({Start,End,BR}/[[_, IV, AS]]>>(
                          format(atom(IK), "invariants:~w-~w:~w:~w:~w", [Start,End, IV, BR, AS]),
                          redis(rd, sadd(IK, K))), Ans))
                       , Ks1).

% run with ./redis_reindex_results.pl  | pv -l
main([RedisIP,TFrom, TTo]) :-
    atom_number(TFrom, Start),
    (TTo = inf -> End = inf ; atom_number(TTo, End)),
    redis_invariants(RedisIP:6379, [], Start, End), !.
main([RedisIP]) :-
    redis_invariants(RedisIP:6379, [], 0, inf), !.
