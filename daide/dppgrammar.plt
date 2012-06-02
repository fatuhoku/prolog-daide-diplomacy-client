% Unit tests for the dppgrammar
%
% Simple bi-directional tests have been employed here to ensure
% that an encoding and decoding of terms to tokens is bijective.

:- ensure_loaded('../daide/dppgrammar').
:- begin_tests(dppgrammar).

% Put in an empty string to signify full parse.
cs_msg_adapter(Term, Tokens):-
  cs_msg(Term,Tokens,[]).
sc_msg_adapter(Term, Tokens):-
  sc_msg(Term,Tokens,[]).

% a basic round-trip test for the important cs_msgs.
test(cs_msg):-
  Unit = unit('ENG', 'AMY', 'LVP'),
  Terms = [
    nme('Test Name','0.1'),
    obs,
    iam('AUS',8191),
    iam('ENG',-8192),
    map,
    mdf,
    yes(map('AWESOME')),
    rej(sve('save game name')),
    now, 
    sco,
    hst(turn('AUT', 1902)),
    tme(215),
    tme,
    adm('Admin', 'ROCKS'),
    hlo,
    sub(hld(Unit)),
    sub(mto(Unit,'LDN'))
    ],
  maplist(cs_msg_adapter, Terms, TokensLists),
  maplist(cs_msg_adapter,NewTerms, TokensLists),
  print(Terms), print(NewTerms),
  Terms == NewTerms.

% Test server-client messages
test(sc_msg):-
  Terms = [
    hlo('ENG',1234,[lvl(20),mtl(1200)])
  ],
  maplist(sc_msg_adapter, Terms, TokensLists),
  maplist(sc_msg_adapter,NewTerms, TokensLists),
  Terms == NewTerms.

:-end_tests(dppgrammar).