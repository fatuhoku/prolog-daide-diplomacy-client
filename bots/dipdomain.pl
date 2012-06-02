%
% In this file, we will specify all of the dynamic knowledge
% predicates, and what they mean within the game.
% They are listed below:

% ally/2  Power1, Power2   -> Power1 is an ally of Power2 
% power/1 Power      -> Power is a valid great power.
% sc/1    Province   -> Province is a supply centre.
% home/2  Power, SupplyCentre   -> SupplyCentre is a home centre of Power.
% prov/1  Province   -> Province is a province.
% has/2   Province, Adjs -> Province has Adjs as adjacencies. Adjs is in the format [can_move/2] (following)
% unit/3  Power,Type,Location  -> A unit of type Type belonging to power Power is standing at location Location.
% unit/4  Power,Type,Location,Mrts -> A unit of type Type belonging to power Power is standing at the location, with a must-retreat list.
%  + can_move/2  Type, [Locations]   -> units of type Type are able to go to any of Locations from Province.
% on top of the above predicates, a layer of indirection may be introduced to do more
% advanced stuff.

:- dynamic ally/2.
% TODO make sure ownage is asserted correctly! :- dynamic owns/2
:- dynamic power/1.
:- dynamic unit/3.
:- dynamic unit/4. 
:- dynamic sc/1.
:- dynamic prov/1.

% is_bicoastal/1
%
% Holds if the given Province is known to have two coasts
%
% +Province
% 
is_bicoastal(Province):-
  location(loc(Province,_)).

% is_coastal/1
%
% Holds if the given Province is bi-coastal, or has some sort of
% FLT adjacencies and some sort of AMY adjacencies.
%
% TODO consider using land(), sea() predicates.
%
is_coastal(Province):-
  is_bicoastal(Province),!.
is_coastal(Province):-
  can_move('AMY', loc(Province), _),
  can_move('FLT', loc(Province), _).

% is_sea/1
%
% Holds if the given Province is a sea. This is defined by
% \E FLT adjacencies AND ¬\E AMY adjacencies.
%
is_sea(Province):-
  is_bicoastal(Province),!.
is_sea(Province):-
  can_move('FLT', loc(Province), _),
  \+ can_move('AMY', loc(Province), _).
  
% is_land/1
%
% Holds if the given Province is a land province. This means that
% AMY units may convoy to it, and stand in it. This is defined as
% NOT is_sea.
%
is_land(Province):-
  \+ is_sea(Province),!.

% prov_of/2
%
% Unifies province of a location with the location.
%
% ?Location
% ?Province
%
prov_of(loc(Province), Province):-
  location(loc(Province)).
prov_of(loc(Province,Coast), Province):-
  location(loc(Province,Coast)).

% location/1
%
% Unifies with every single location in the game. A location has the syntax:
% loc(Prov) OR loc(Prov,Coast) where Prov is a valid province, and Coast is a
% valid coast for that province.
% Even though has/2 asserted predicate is definitive to the map definition,
% we will rely on the prov/1 asserted predicate to find whether it's valid.
% Validity of coasts relies on has/2 via can_move/3.
%
% -Loc location
%
location(loc(Prov)):- 
  prov(Prov).
location(loc(Prov,Coast)):-
  can_move('FLT', loc(Prov,Coast), _).

% at/2
%
% Unifies a province with a unit on it.
% Overcomes the abstraction of Province and Coast vs. just Province when querying.
%
% ?Prov the province Unit is standing on
% ?Unit the unit at Prov
%
at(Prov,unit(Power,Type,Loc)):-
  prov_of(Loc,Prov),
  unit(Power,Type,Loc).
at(Prov,unit(Power,Type,Loc,Mrt)):-
  prov_of(Loc,Prov),
  unit(Power,Type,Loc,Mrt).

% can_stand/2
% 
% Says that a type of unit may stand in a particular province.
% This is the case if some province allows some kind of movement from it.
%
% ?Type a unit type 'AMY' or 'FLT'
% ?Loc  a province in the form loc(Province)
%
can_stand('AMY', loc(Prov)):-
  !, is_land(Prov).
can_stand('FLT', Loc):-
  can_move('FLT', Loc, _).

% can_move/3
% 
% Says that a unit may move into another province.
% This is done by examining the has/2 asserted predicates.
%
% ?UnitOrType a unit in form unit/3 or unit/4 or a Type in form 'AMY' or 'FLT'
% ?Loc1 either loc(Province) or loc(Province,Coast)
% ?Loc2 either loc(Province) or loc(Province,Coast)
%
can_move(unit(_Power,Type,Loc1),Loc1,Loc2):-
  can_move_type(Type,Loc1,Loc2).
can_move(unit(_Power,Type,Loc1,_Mrt),Loc1,Loc2):-
  can_move_type(Type,Loc1,Loc2).
can_move(Type,Loc1,Loc2):-
  can_move_type(Type,Loc1,Loc2).

% can_move_type/3
% 
% Says that a type of unit may move into another province.
% Try using 'can_move' as a wrapper to this.
% This is done by examining the has/2 asserted predicates.
%
% ?Type a unit type 'AMY' or 'FLT'
% ?Loc1 either loc(Province) or loc(Province,Coast)
% ?Loc2 either loc(Province) or loc(Province,Coast)
%  
can_move_type('FLT', loc(Prov1,Coast), Loc2):-
  has(Prov1, Adjs),
  member(can_move('FLT'(Coast), Locs), Adjs),
  member(Loc2,Locs).
can_move_type('FLT', Loc1, loc(Prov2,Coast)):-
  has(Prov2, Adjs),
  member(can_move('FLT'(Coast), Locs), Adjs),
  member(Loc1,Locs).
can_move_type(Type,loc(Prov1),Loc2):-
  has(Prov1, Adjs), !,
  member(can_move(Type, Locs), Adjs),
  member(Loc2, Locs).
can_move_type(Type,Loc1,loc(Prov2)):-
  has(Prov2, Adjs), !,
  member(can_move(Type, Locs), Adjs),
  member(Loc1, Locs).
  
% neighbours/2
%
% Unifies Neighours with the neighbouring provinces of Province, regardless of 
%
% +Province
% -Neighbours
%
% neighbours(Prov, Neighbours):-
%  prov(Prov),
%  findall(Loc, can_move(_,Location,Loc), Bag),
%  list_to_ord_set().

% vacant/2
%
% Holds if Loc has no units on it.
% +Loc
%
vacant(Loc):-
  location(Loc), !,
  \+ at(Loc,_).
vacant(Province):-
  prov_of(Loc, Province),
  \+ at(Loc,_).

% unit_count/2
%
% Unifies Count with the number of units the Power currently owns
% +Power
% -Count
unit_count(Power,Count):-
  power_units([Power],List),
  length(List,Count).

% units/1
% 
% Unifies Units with a list of all the units in the game.
% -Units a list of all the units on the board.
%
units(Units):-
  findall(unit(P,T,L), unit(P,T,L), UnitBag1),
  findall(unit(P,T,L,M), unit(P,T,L,M), UnitBag2),
  append(UnitBag1, UnitBag2, Units).

% powers_units/2
% 
% Unifies Units with a list of all the units that belong to the powers specified.
% 
% +Powers a list of powers.
% -Units all the units of the power.
%
powers_units([], []).
powers_units([Power|Powertail],Units):-
  findall(unit(Power,T,L), unit(Power,T,L), UnitBag1),
  findall(unit(Power,T,L,M), unit(Power,T,L,M), UnitBag2),
  powers_units(Powertail, MoreUnits),
  append([UnitBag1,UnitBag2,MoreUnits], Units).

% power_units/2
% 
% An adapter for powers_units for a single power.
% 
% +Power a power
% -Units all the units of the power.
%
power_units(Power,Units):-
  powers_units([Power],Units).

% ally_units/2
%
% Unifies Units with a list of allies' units.
% -Units
%
ally_units(Units):-
  ally_powers(Allies), powers_units(Allies,Units).

% enemy_units/2
%
% Unifies Units with a list of enemy units.
% -Units
%
enemy_units(Units):-
  enemy_powers(Powers), powers_units(Powers,Units).
  
% my_units/2
%
% Unifies Units with a list of my units.
% -Units
%
my_units(Units):-
  this_power(Power), powers_units([Power],Units).

% min_path/4
% 
% Calculates the minimum number of moves required for a particular type of unit to
% reach Loc2 from Loc1. This uses the double-sided wavefront algorithm: 
% The idea is that we keep two sets: these sets represent the 'ripples' that spread out
% from Loc1 and Loc2. These shall be called RipX and PleX respectively, where X is the
% number of tempi away.
% 
% The Diplomacy map is finite, and does not form a connected graph, and therefore
% there needs to be some sort of stopping condition: when RipN or PleN becomes [],
% then we have failed to find a path!
%
% Rip0 will start as [Loc1], Ple0 will start as [Loc2].
% 
% 1) consider the next ripple from RipN. 
%    Subtract this set by RipN-1, and we have RipN+1.
% 2) check whether Loc2 is in RipN+1. IF so, stop.
% 3) consider the provinces one tempo away from all provinces in PleN. This is PleN+1.
% 4) attempt to find P s.t. member(P,RipN), member(P,PleN).
%    If successful, add all bindings of P as a list to our list of known nodes.
%    IF not, then recurse with RipN as starting point.
% 5) Append together the result of min_path(Type, Loc1, P, Path1) and
%    min_path(Type,Loc2,P,Path2) to produce a complete list of paths.
%
% This operation is guaranteed to find the shortest path between Loc1 and Loc2.
% 
% ?Type the unit type
% ?Loc1 the location to go from
% ?Loc2 the location to go to
% -Path the provinces that will have to be travelled through.
%
% The Path list looks like this:
% [[Province, Province]...,[Province],...]
% ... where the head (first sublist) corresponds to the provinces that take you to Loc2
% that is one tempo away, and each subsequent sublist corresponds to the provinces that
% lie on the shortest path a certain number of tempi away.
% The tail of the list is always [Loc2], as to say the final move is to move there.

min_path_opposites(rip,ple).
min_path_opposites(ple,rip).

min_path(Type,Loc1,Loc1,[]):-
  !, can_stand(Type,Loc1).
min_path(Type,Loc1,Loc2,Path):-
  location(Loc1), location(Loc2),
  min_path_expand(Type, Loc1, Loc2, [], [Loc1], [], [Loc2], rip, Path).
  
% min_path_expand/9
% 
% Expands one of Rip or Ple, depending on what's defined for Rp.
%
% +Type  Unit type
% +Loc1  From
% +Loc2  To
% +OldRip  the older Rip
% +Rip     the current Rip
% +OldPle  the older Ple
% +Ple     the current Ple
% +Rp    which expansion to run
% -Path  the path.
%
min_path_expand(Type, Loc1, Loc2, OldRip, Rip, OldPle, Ple, rip, Path):-
  ripple(Type, OldRip, Rip, NewRip),
  ord_intersection(NewRip, Ple, Ps),
  min_path_check(Type, Loc1, Loc2, Rip, NewRip, OldPle, Ple, Ps, rip, Path).
min_path_expand(Type, Loc1, Loc2, OldRip, Rip, OldPle, Ple, ple, Path):-
  ripple(Type, OldPle, Ple, NewPle),
  ord_intersection(Rip, NewPle, Ps),
  min_path_check(Type, Loc1, Loc2, OldRip, Rip, Ple, NewPle, Ps, ple, Path).

% min_path_check/10
% 
% Checks whether we're finally done, or whether two provinces are unreachable.
% 1) if Common == [] then we can proceed to expand.
% 2) if Rip or Ple == [] then we have failed to find a path. Fail.
% If there is, then we bind Path to that stuff.
% Rp is an option that lets min_path_check alternate between expanding Rip or Ple.
%
min_path_check(_Type,_Loc1,_Loc2,_OldRip,[],_OldPle,_Ple,_Common,_Rp, _Path):-
  !, fail.
min_path_check(_Type,_Loc1,_Loc2,_OldRip,_Rip,_OldPle,[],_Common,_Rp, _Path):-
  !, fail.
min_path_check(Type, Loc1, Loc2, OldRip, Rip, OldPle, Ple, [], Rp, Path):-
  min_path_opposites(Rp,NextRp),
  min_path_expand(Type, Loc1, Loc2, OldRip, Rip, OldPle, Ple, NextRp, Path).
% BASE CASE: when Loc2 appears in Common, we know we've got there.
min_path_check(_Type, _Loc1, Loc2, _OldRip,_Rip,_OldPle,_Ple, Common, _Rp, [Common]):-
  member(Loc2, Common),!.
min_path_check(_Type, Loc1, Loc2,_OldRip,_Rip,_OldPle,_Ple, Common,_Rp, Path):-
  findall(Path1, (member(C,Common), min_path(Type,Loc1,C,Path1)), Path1s),
  set_multi_union(Path1s, FirstPaths),
  findall(Path2, (member(C,Common), min_path(Type,C,Loc2,Path2)), Path2s),
  set_multi_union(Path2s, SecondPaths),
  append(FirstPaths,SecondPaths,Path).
  
% set_multi_union/2
%
% Let the argument be Arg, and its elements Arg[N]. Each of these elements are lists of sets-
% let's call these sets Arg[N][X]. They have some fixed length K.
% Let the resulting list be Res, and its elements Res[N].
% Then the following holds:
%  for 0 <= N < length(Arg), for every K < length(Arg[0])
% foldr(ord_union, Arg[N][K]) = Res[K]
%
% We do this by taking two columns and maplisting ord_union upon it. Repeat
% until there remains only one column.
% A union on no sets is not valid.
%
% +Stuff
% -Res
%
set_multi_union( [Set], Set ).
set_multi_union( [Set1,Set2|Sets], Result ):-
  maplist(ord_union, Set1, Set2, Merged),
  set_multi_union([Merged|Sets], Result).

% ripple/3
%
% Finds the next ripple of provinces.
% +Type
% +OldRipple
% +CurrentRipple
% -NewRipple
ripple(Type, Old, Curr, New):-
  findall(Dest, (member(Loc,Curr), can_move(Type,Loc,Dest)), RipBag),
  list_to_ord_set(RipBag, RipSet),
  ord_subtract(RipSet, Old, Int),
  ord_subtract(Int, Curr, New).



%%%%%% HEURISTICS %%%%%%%%

% Simple heuristics will come in the form:
% [min|max]_[offensive|defensive|threat]_strength

% min_defensive_strength/2
%
% A simple measure of how much defensive strength there IS on a province.
% A province will have defensive strength 1 if there exists a unit on it, and 0 otherwise.
% This predicate is power-neutral. (can be used for any province)
%
% +Location  the location: must be province in form loc(Prov)
% -MinDefStrength 
min_defensive_strength(loc(Prov),1):-
  at(_, Prov). % some unit is present at Prov.
min_defensive_strength(loc(Prov),0):-
  prov(Prov).

% max_defensive_strength/2
%
% This is defined to be:
% (
% IF I own this province,
% THEN the count of *friendly* units that may move into this province (i.e. can support a hold)
% ELSE the count of *enemy* units may move into this province
% (i.e. how an enemy may support a hold, or move into a 'neutral' province)
% )
% PLUS the min_defensive_strength of this province.
% 
% This is power-sensitive.
% +Location  the location: must be province in form loc(Prov)
% -MaxDefStrength 
% TODO consider requesting allies' help when calculating maximum defensive strength.
% ONLY if upon successful Diplomatic agreement.
%
max_defensive_strength(loc(Prov),Strength):-
  max_defensive_strength_aux(Prov, MaxAux),
  min_defensive_strength(Prov,Min),
  plus(MaxAux,Min,Strength).
max_defensive_strength_aux(loc(Prov),Influx):-
  owns(Power,Prov), this_power(Power),
  powers_units(Power,Units),  % all the power's units
  findall(Unit, ( member(Unit,Units), can_move(Unit,_,Prov) ), UnitsBag),
  length(UnitsBag,Influx).
max_defensive_strength_aux(loc(_Prov),_Influx):-
  fail.
  
  
% Powers should consider SupplyCentres to be increasingly more important if it owns more provinces.

%%%% Some extra tokens %%%%
coast('NCS').
coast('NEC').
coast('ECS').
coast('SEC').
coast('SCS').
coast('SWC').
coast('WCS').
coast('NWC').