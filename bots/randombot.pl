% holdbot extends trivialbot:
% 1) It connects to the server, and sends the initial message.
% 2) It will then continually wait for incoming messages, and deal with them in the following
% way:
%   NOW message - holdbot looks for all the units that it 'owns', and submits orders
%                 for all of those units to hold.
%   Delegate all messages to botbase.
% Sends a nme message with the name 'trivialbot', version v0.1.
%
:-consult('botutil').
:-ensure_loaded(library(random)).

dip_randombot:-
  dip_connect(localhost,16713),
  dip_send_init,
  repeat,
  dip_receive(Msg),
  handle(Msg),
  fail.

handle(msg_rep(X)):-
  log(debug,X), nl,
  dip_send_dip(nme('Hok\'s holdbot','v0.1')).

% 1) Get the power we're playing as
% 2) Find all of this power's units.
% 3) Find all the possible moves for these units including hold
% 4) Randomly choose one of these options for that unit
% 5) Submit the orders.
handle(msg_dip(now(turn(Season,Year), Units))):-
  concat_atom([Season,' ',Year,': RandomBot is thinking...'], LogMsg),
  log(info, LogMsg),
  my_units(Units),  
  maplist(choose_valid_order,Units,Orders),
  dip_send_dip(sub(Orders)).

% choose_random_valid_order
%
% Returns a random valid order for a particular unit
%
choose_random_valid_order(Unit, Order):-
  valid_orders(Unit,Orders),
  random_member(Orders,Order). % imported from util

% valid_order/2
%
% Unifies a valid movement- or retreat-phase order with the given unit.
% Valid orders do not consider the validity of the move in terms of
% diplomatic relations, bouncing, or entering an occupied area: 
% this ulterior logic belongs in High Command.
% 
% Valid orders include:
%
% +Unit in the form of unit/3 or unit/4
% -Order a valid order given the current position, without consideration of
%  any orders another unit may be making.
%
% Movement phase orders
% 1) Hold (any unit may hold)
% 2) Move to a neighbouring province
% 3) Supporting hold of a neighbouring unit
% 4) Supporting of attack of a neighbouring province by a unit also
%      neighbouring that province
% 5) Convoy a unit (CVY) to a province
% 6) Move by convoy (CTO) to a province
%
% HUnit implies a holding unit
% MUnit implies a moving unit
% SUnit implies a supporting unit
% CFlt  implies a fleet for convoy
% LAmy  implies an army to be convoyed as Load
% To implies the target location of a movement or convoy
% From implies the source location of a movement or convoy
% 
% All the fields of the above are also prefixed by the letter H,M,S,C appropriately.
%
valid_order(HUnit, hld(HUnit)). % Holdunit is Holding!
valid_order(MUnit, mto(MUnit, To)):-
  MUnit = unit(_MPower,MType,MFrom),
  can_move(MType,MFrom,To).
valid_order(SUnit, sup(SUnit, HUnit)):-
  SUnit = unit(_SPower,SType,SLoc),
  can_move(SType,SLoc,HLoc), 
  at(HLoc, HUnit).
valid_order(SUnit, sup(SUnit, mto(unit(MPower,MType,From), To))):-
  SUnit = unit(_SPower,SType,SLoc),
  can_move(SType,SLoc,To),
  can_move(MType,To,From),
  at(From, unit(MPower,MType,From)).

% Note: corresponds to (unit) CVY (unit) CTO (province)
%
% TODO implement the finding of valid convoy orders
%
% We shall use the yet another rippling algorithm to consider what
% possibilities we have of convoying units across seas correctly.
%
% Starting with a FLT
% Keeping a set of adjacent and unique set of fleets, starting from [CFlt]
% Keeping a set of land provinces that can be reached by this chain, starting with []
%
% 1) we find the largest 'chain' (or 'blob') of fleets on seas.
% 2) we find all of the land provinces that this chain can reach.
% 3) we find any units on these land provinces and bind valid orders
%    to every destination.
% 4) 
%
% valid_order(CFlt, cvy(CFlt, cto(LAmy, To))):-
%  CFlt = unit(CPower,'FLT',CFrom),
%  is_sea(CFrom),
%  convoy_chain_landprovs(CFlt, Chain, LProvs),
%  member(LProv, LProvs),
%  at(LProv, LAmy),
%  member(To, LProvs), To \== LProv.
  
% valid_order(LAmy, cto(LAmy, To, via(Seas))):-
%  LAmy = unit(LPower,'AMY', LFrom),
%  fail.
  
% Retreat phase orders
%
% 1) mrt list and order an rto to it.
% 2) Disband if mrt list is empty.
%
valid_order(Unit, rto(Unit, RProv)):-
  Unit = unit(_Power,_Type,_From,mrt(RProvs)),
  member(RProv, RProvs).
valid_order(Unit, dsb(Unit)):-
  Unit = unit(_Power,_Type,_From,mrt([])).

% valid_build_order/2
%
% Unifies with all the valid build orders for a particular home centre
% of a particular power.
%
% +Province the province we're considering of building something in
% -Order a valid build order at build phase.
%
% Build phase orders
% 1) Build a unit at a home supply centre.
%
valid_build_order(Province, bld(unit(Power,Type,Location))):-
  is_bicoastal(Province), vacant(Province),
  home(Power, Province), sc(Province),
  valid_build_order_aux(Province,Type,Location). % (*)
% NOT BICOASTAL CASE:
valid_build_order(Province, bld(unit(Power,Type,loc(Province)))):-
  vacant(Province), home(Power, Province), sc(Province),
  member(Type,['AMY','FLT']).
  
% (*)  
valid_build_order_aux(Province,'FLT',loc(Province,Coast)):-
  location(loc(Province, Coast)).
valid_build_order_aux(Province,'AMY',loc(Province)):-
  location(loc(Province)).



% valid_remove_order/2
%
% Unifies with all the valid unit removal orders for a particular power.
%
% +Power the power we're trying to remove a unit for.
% -Order a valid build order at build phase.
%
% Build phase orders
% 2) Remove a unit due to insufficient supply centres
%
valid_remove_order(Power, rem(Unit)):-
  units([Power], Units),
  member(Unit,Units).
  
% valid_waive_order/2
%
% Unifies with valid waive orders for a particular power.
% Note: the cctual number of waive orders required is not considered here.
%
% +Power the power we're trying to waive build orders for
% -Order a valid build order at build phase.
%
% Build phase orders
% 3) Waive build due to insufficient supply centres to build more units
%
valid_waive_order(Power, wve(Power)).

:-consult(botbase).
