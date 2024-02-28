#!/usr/bin/env -S -- swipl -q --stack-limit=8G -g main -t halt

:- use_module(library(main)).
:- use_module(library(gensym)).
:- use_module(library(http/json)).
:- use_module(sexpr).

:- dynamic url_parse/2.
:- dynamic domain_site/2.
:- dynamic split_cookie/2.


take(N, List, Prefix) :- length(Prefix, N), append(Prefix, _, List).
hostname_site(H, Site) :-
    re_split("\\.", H, L),
    reverse(L, R), take(3, R, S), reverse(S, O),
    atomics_to_string(O, Site).

hostname_site("localhost", "localhost").

convert_url(json(Ls), Hr, 'build-url'(Hr, Hst, Hstnm, Orig, Pth, Prt, Proto, Site)) :-
    member(href=Hr0, Ls), atom_string(Hr0, Hr),
    member(host=Hst0, Ls), atom_string(Hst0, Hst),
    member(hostname=Hstnm0, Ls), atom_string(Hstnm0, Hstnm),
    member(origin=Orig0, Ls), atom_string(Orig0, Orig),
    member(pathname=Pth0, Ls), atom_string(Pth0, Pth),
    member(port=Prt0, Ls), atom_string(Prt0, Prt),
    member(protocol=Proto0, Ls), atom_string(Proto0, Proto),
    member(site=Site0, Ls), atom_string(Site0, Site),
    asserta(url_parse(Hr, 'build-url'(Hr, Hst, Hstnm, Orig, Pth, Prt, Proto, Site))).
convert_url(Url, _, _) :-
    (atom(Url) ; string(Url)), throw(error(wrong_url(Url),_)).


odd_elems([], []) :- !.
odd_elems([ X ], [ X ]) :- !.
odd_elems([ X, _ ], [ X ]) :- !.
odd_elems([ X, _ | Zs0 ], [X | Zs]) :- odd_elems(Zs0, Zs).

split(Re, Str, L) :- re_split(Re, Str, L0), odd_elems(L0, L).

convert_cookie(A, C) :-
    atom_string(A, C),
    split("\s*;\s*", C, L),
    maplist([X,X1]>>re_replace("^\\s*|\\s*$"/g, "", X, X1), L, L1), % trim
    maplist([X0,X]>>(re_replace("^secure$"/i, "Secure", X0, X1),
                     re_replace("^samesite=lax$"/i, "SameSite=Lax", X1, X2),
                     re_replace("^samesite=strict$"/i, "SameSite=Strict", X2, X3),
                     re_replace("^samesite=none$"/i, "SameSite=None", X3, X4),
                     re_replace("^path=(.*)$"/i, "Path=\\1", X4, X5),
                     re_replace("^domain=(.*)$"/i, "Domain=\\1", X5, X6),
                     re_replace("^httponly$"/i, "HttpOnly", X6, X)), L1, L2),
    asserta(split_cookie(C, L2)).

convert_event(Ts=json(Ls), Ts='js-set-cookie'(ctx(Id, Url, Sc), Set, "")) :-
    member(type='DocumentCookieSet', Ls),
    (    member(isSecure=Sc0, Ls), convert_bool(Sc0, Sc),
         member(wid=Id0, Ls), atom_string(Id0, Id),
         member(orig=Url0, Ls), convert_url(Url0, Url, _),
         member(args=[Set0], Ls), convert_cookie(Set0, Set)
    ;    throw(error(wrong_event(Ls),_))
    ).

convert_event(Ts=json(Ls), Ts='js-get-cookie'(ctx(Id, Url, Sc), Res)) :-
    member(type='DocumentCookieGet', Ls),
    (    member(isSecure=Sc0, Ls), convert_bool(Sc0, Sc),
         member(wid=Id0, Ls), atom_string(Id0, Id),
         member(orig=Url0, Ls), convert_url(Url0, Url, _),
         member(ret=Res0, Ls), convert_cookie(Res0, Res)
    ;    throw(error(wrong_event(Ls),_))
    ).

convert_event(Ts=json(Ls), Ts='net-request'(Id, Url, Method, RType, OriginUrl, DocumentUrl, FrameAncestors, Redirections,
                                            'request-headers'(Cookie, OriginHd), ReqBody)) :-
    select(type='Request', Ls, Rs),
    (    member(requestId=Id0, Ls), atom_string(Id0, Id),
         ( member(type=RType0, Rs) -> convert_resource_type(RType0, RType) 
         ; RType = 'type-other' ),
         member(method=Method0, Ls), convert_method(Method0, Method),
         ( member(redir=Redir, Ls) ->
           maplist([In,Out]>>(convert_url(In, Out, _)), Redir, Redir0),
           list_smtlist(Redir0, Redirections)
         ; Redirections = nil ),
         ( member(frameAncestors=Fa, Ls) ->
           maplist([json(In1),Out1]>>(member(url=Url3, In1), convert_url(Url3, Out1, _)), Fa, Fa0),
           list_smtlist(Fa0, FrameAncestors)
         ; FrameAncestors = nil ),
         ( member(originUrl=OriginUrl0, Ls) -> convert_url(OriginUrl0, OriginUrl, _)
         ; OriginUrl = ""
         ),
         ( member(documentUrl=DocumentUrl0, Ls) -> convert_url(DocumentUrl0, DocumentUrl1, _), DocumentUrl = some(DocumentUrl1)
         ; DocumentUrl = none
         ),
         member(url=Url0, Ls), convert_url(Url0, Url, _),
         member(requestHeaders=Hds, Ls),
         ( ( member(json(Co), Hds), member(name='Cookie', Co) )
         -> member(value=Vo, Co), convert_cookie(Vo, V1), Cookie = some(V1)
         ; Cookie = none),
         ( ( member(json(Or), Hds), member(name='Origin', Or) )
         -> member(value=V2, Or), atom_string(V2, V3), OriginHd = some(V3)
         ; OriginHd = none),
         ((member(body=ReqBody0, Ls), ReqBody = some(ReqBody0)) ; ReqBody = none)
    ;    throw(error(wrong_event(Ls),_))
    ).

convert_event(Ts=json(Ls), Ts='net-response'(Id, Url, 'response-headers'(Cookie), ResBody)) :-
    member(type='Response', Ls),
    (    member(requestId=Id0, Ls), atom_string(Id0, Id),
         member(url=Url0, Ls), convert_url(Url0, Url, _),
         ( member(responseHeaders=Hds, Ls) ->
           % cookies
           convlist([json(Hd),C]>>(member(name='Set-Cookie', Hd), member(value=V0, Hd), atom_string(V0, C)), Hds, Cookie0),
           maplist(split("\n"), Cookie0, Cookie1), flatten(Cookie1, Cookie2),
           maplist(convert_cookie, Cookie2, Cookie3),
           list_smtlist(Cookie3, Cookie)
         ; Cookie = nil ),
         % body
         ((member(body=ResBody0, Ls), ResBody = some(ResBody0)) ; ResBody = none)
    ;    throw(error(wrong_event(Ls),_))
    ).

convert_event(Ts=json(Ls), Ts='js-fetch'(ctx(Id, Url, Sc), Target)) :-
    member(type='Fetch', Ls),
    (    member(isSecure=Sc0, Ls), convert_bool(Sc0, Sc),
         member(wid=Id0, Ls), atom_string(Id0, Id),
         member(orig=Url0, Ls), convert_url(Url0, Url, _),
         member(args=[Target0 | _], Ls), atom_string(Target0, Target)
    ;    throw(error(wrong_event(Ls),_))
    ).

convert_event(Ts=json(Ls), Ts='cookie-jar-set'(Name, Value, 'cookie-attrs'(Secure, HttpOnly, SameSite, Path, Domain))) :-
    member(type='CookieJar', Ls),
    member(removed=(@(false)), Ls),
    (
         member(cookie=json(C), Ls),
         member(name=Name0, C), atom_string(Name0, Name),
         member(value=Value0, C), atom_string(Value0, Value),
         member(secure=Secure0, C), convert_bool(Secure0, Secure),
         member(httpOnly=HttpOnly0, C), convert_bool(HttpOnly0, HttpOnly),
         member(sameSite=SameSite0, C), convert_samesite(SameSite0, SameSite),
         member(path=Path0, C), atom_string(Path0, Path),
         member(domain=Domain0,C), atom_string(Domain0, Domain),
         ( hostname_site(Domain, Site), asserta(domain_site(Domain, Site)) )
    ;    throw(error(wrong_event(Ls),_))
    ).

convert_bool(@(true), true).
convert_bool(@(false), false).

convert_samesite(unspecified, 'SS-None').
convert_samesite(no_restriction, 'SS-None').
convert_samesite(lax, 'SS-Lax').
convert_samesite(strict, 'SS-Strict').

convert_resource_type(RType0, RType) :- atom_concat('type-', RType0, RType).

convert_method('GET','M-GET') :- !.
convert_method('POST','M-POST') :- !.
convert_method('PUT','M-PUT') :- !.
convert_method('DELETE','M-DELETE') :- !.
convert_method('OPTIONS','M-OPTIONS') :- !.
convert_method('PATCH','M-PATCH') :- !.
convert_method(_,'M-OTHER').


assertion(Goal) :- Goal, !.
assertion(Goal) :- throw(error(assertion_error(Goal),_)).

group_by(_, [], []).
group_by(_, [A], [[A]]).
group_by(P, [A|Ls], R) :-
    group_by(P, Ls, [[G|G1s]|Gs]),
    ( call(P, A, G) ->  R = [[A,G|G1s]|Gs]
    ; R = [[A],[G|G1s]|Gs] ).


reindex_network(Type, _=json(Ls), Ts=json([type=Type|Ls])) :-
    reindex_event(json(Ls), Ts=json(Ls)).

reindex_event(json(E), Ts=json(E)) :-
    assertion(member(timeStamp=Ts0, E)),
    Ts1 is truncate(Ts0),
    atom_number(Ts,Ts1).

flatten_cookies(Ls, Fs) :-
    maplist([Ts=L, Fs]>>(
                maplist({Ts}/[json(A),Ts=json([type='CookieJar'|A])]>>true, L, Fs)
            ), Ls, Ns),
    append(Ns, Fs).

add_network_body(ProxyJson, Ts=json(NetJson), Ts=json([body=BodyS|NetJson])) :-
    member(json(ProxyNet), ProxyJson),
    member(timeStamp=ReqTimeStamp, NetJson),
    member(url=Url, NetJson),
    member(url=Url, ProxyNet),
    ( member(type='Request', NetJson) ->  member(request_start=StartTs, ProxyNet)
    ; member(type='Response', NetJson) -> member(response_start=StartTs, ProxyNet)),
    abs(ReqTimeStamp - (StartTs * 1000)) < 100,
    member(body=Body, ProxyNet),
    atom_string(Body, BodyS),
    BodyS \= "", !.
add_network_body(_, NetJson, NetJson).


list_events(Ls, E) :-
    % Network
    assertion(member(network=json(N0), Ls)),
    assertion(member(requests=json(R0), N0)), maplist(reindex_network('Request'), R0, R1),
    assertion(member(responses=json(P0), N0)), maplist(reindex_network('Response'), P0, P1),
    % Proxy
    ( member(proxy=json(Proxy), Ls) ->
        ( assertion(member(requests=PR0, Proxy)),
          assertion(member(responses=PP0, Proxy)),
          maplist(add_network_body(PR0), R1, R2),
          maplist(add_network_body(PP0), P1, P2) )
    ; R2 = R1, P2 = P1 ),
    % Cookies
    assertion(member(cookies=json(C0), Ls)), flatten_cookies(C0, C1),
    % JS Events
    assertion(member(events=E0, Ls)), maplist(reindex_event, E0, E1),
    % Sort
    append([E1, R2, P2, C1], Evs0),
    maplist([T=V,TN=V]>>atom_number(T,TN), Evs0, Evs),
    sort(1, @=<, Evs, E).


% split groups so that cookie-jar-set evnts are not mixed with other events
split_cookiejar_set_groups([], []).
split_cookiejar_set_groups([G|Gs], O) :-
    convlist([T=E,T=E]>>(E='cookie-jar-set'(_, _, _)), G, CJs),
    convlist([T=E,T=E]>>(E\='cookie-jar-set'(_, _, _)), G, Others),
    split_cookiejar_set_groups(Gs, Rest),
    Out = [ Others | Rest ],
    ( CJs = [] -> O = Out ; O = [ CJs | Out ] ).

permute_simultaneous(Ls, R) :-
    group_by([T=_, T=_]>>true, Ls, Gs0),
    split_cookiejar_set_groups(Gs0, Gs),
    % do not permute cookie-jar-set
    maplist([A, Out]>>( maplist([T='cookie-jar-set'(_, _, _)]>>true, A)
                      -> Out=A ; Out=A), Gs, Ps),
    append(Ps, R).

list_converted_events(E, C) :-
    convlist(convert_event, E, Es),
    permute_simultaneous(Es, C).


list_tests(Ls, Ts) :-
    member(tests=json(Tests), Ls),
    list_events(Ls, E),
    foldl([_=json(S), Old, Min]>>(member(start=St, S), Min is min(Old, St)), Tests, inf, Start0),
    ( Start0 = inf ->
      findall(Es, list_converted_events(E, Es), Te1),
      Ts = [ "All Events"-Te1 ]
    ; maplist({Start0, E}/[Name=json(Se),SName-Te1]>>(
                  atom_string(Name,SName),
                  assertion(member(start=Start, Se)),
                  assertion(member(end=End, Se)),
                  convlist({Start,End}/[T=Ev,T=Ev]>>((T >= Start, T =< End) ; T =< Start0), E, Te),
                  findall(Es, list_converted_events(Te, Es), Te1)
              ), Tests,Ts)
    ).


list_smtlist([], nil).
list_smtlist([X|Xs], insert(X,Ls)) :- list_smtlist(Xs, Ls).

term_list(T, A) :- T =.. [A].
term_list(T, Ts) :- T =.. Ts0,
    maplist(term_list, Ts0, Ts).


get_invariants(Smtlib,Invs) :-
    convlist([F,Name-F]>>(F = ['define-fun', Name, [[_, ['List', 'Action']]], 'Bool' |_]), Smtlib, Invs).

invariants_assertions(Sym, Invariants, Assertions) :-
    format(string(SymStr), "===== ~w =====", Sym),
    maplist({Sym,SymStr}/[InvName-_, [['push'], ['echo', SymStr], ['echo', InvNameS], ['assert-not', [InvName, Sym]], ['check-sat'], ['pop']]]>>atom_string(InvName, InvNameS),
        Invariants, Assertions).


main([Definitions0,Filename0]) :-
    atom_string(Filename0, Filename),
    atom_string(Definitions0, Definitions),

    setup_call_cleanup(open(Definitions, read, Stream1),
                       read_string(Stream1, _, DString),
                       close(Stream1)),
    setup_call_cleanup(open(Filename, read, Stream2, [encoding(iso_latin_1)]),
                       json_read(Stream2, json(JsonLs)),
                       close(Stream2)),

    string_chars(DString, DChars),
    (phrase(sexprs(Smtlib), DChars) ; throw(error(parse_error,_))),
    get_invariants(Smtlib, Invariants),

    assertion(list_tests(JsonLs, Tests)),

    % Print traces to stderr
    maplist([Name0-Traces]>>(
                escape_non_print(Name0, Name),
                format(user_error, "% ~w\n", Name),
                maplist([T]>>(
                            maplist(format(user_error, "~q\n"), T),
                            format(user_error, "\n", [])), Traces)), Tests),
    % print defintitions
    maplist([Sexpr]>>(
                list_sexpr(Sexpr,Str),
                format("~w\n\n", Str)
            ), Smtlib),

    %% parse-url definitions
    (setof(U-P, url_parse(U, P), Urls) ; Urls= []),
    maplist([U-P]>>(
                term_list(P, B),
                list_sexpr(['assert', ['=', ['parse-url', U], B]],Str),
                format("~w\n\n", Str)
            ), Urls),
    %% domain-site definitions
    (setof(D-S, domain_site(D,S), Domains) ; Domains = []),
    maplist([D-S]>>(list_sexpr(['assert', ['=', ['domain-site', D], S]], Str),
                    format("~w\n\n", Str)), Domains),
    %% cookie header parsing
    (setof(D-S, split_cookie(D,S), Cookies) ; Cookies = []),
    maplist([D-S]>>(list_smtlist(S, S1), term_list(S1, S2),
                    list_sexpr(['assert', ['=', ['split-cookie', D], S2]], Str),
                    format("~w\n\n", Str)), Cookies),

    % print traces
    maplist([Name0-Traces]>>(
                escape_non_print(Name0, Name),
                format("; TEST: ~w\n\n", [Name]),
                maplist([T0]>>(
                            maplist([_=A,A]>>true,T0, T), % remove timestamps
                            list_smtlist(T, List),
                            term_list(List, Out),
                            list_sexpr(Out, S0),
                            gensym(trace, Sym),
                            format("(define-fun ~a () (List Action)\n ~w)\n\n", [Sym,S0]),
                            invariants_assertions(Sym, Invariants, Assertions),
                            maplist(maplist(list_sexpr), Assertions, AS0),
                            maplist([A,S]>>atomics_to_string(A, ' ', S), AS0, AS1),
                            atomics_to_string(AS1, '\n', AS),
                            format("~w\n\n", AS)
                        ), Traces),
                nl
            ), Tests).
            

main(A) :-
    domain_error([definitions, trace], A).
