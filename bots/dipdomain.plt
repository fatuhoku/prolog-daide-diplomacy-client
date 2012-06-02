%
% Unit tests for the grammar
% 

:- consult('../bots/dipdomain').
:- begin_tests(dipdomain)

map_name('Test Map').
power('FRA').
power('RUS').
power('ENG').
power('ITA').
power('TUR').
power('AUS').
power('GER').

test(is_bicoastal):-
  is_bicoastal(...).

test(is_sea):-
  is_sea(...).
  
test(is_land):-
  is_land(...).
  
test(prov_of):-
  prov_of(...).

test(location):-
  location(...).
  
test(at):-
  at(...).

test(can_stand):-
  can_stand(...).
  
test(can_move):-
  can_move(...).
  
test(can_move_type):-
  can_move_type(...).
  
test(vacant):-
  vacant(...).
  

:-end_tests(dipdomain).