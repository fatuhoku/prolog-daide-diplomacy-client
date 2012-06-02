% random_member/2
%
% Unifies Member with a randomly chosen member.
% +List some list.
% -Member a randomly chosen member from List
%
random_member(List, Member):-
  is_list(List),
  length(List,Length),
  random(1,Length,X),
  nth1(X,List,Member).