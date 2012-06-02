%
% botbase.pl
%
% This file contains basic message handlers for Prolog bots. These message handlers
% handle messages by generally updating domain predicates, thereby updating the
% information available to the bot. A detailed behavioural description is
% below.
%
% Behavioural Description:
% (because botbase is early in development, ALL messages received will be printed, and
%  the listed action will be carried out).
% 
% msg_rep: does not handle.
% msg_dip:
%  MAP message - always asks for map definition (mdf)
%  MDF message - always asserts the entire mdf using domain predicates power,
%                home, prov, has, can_move etc.
%                always replies yes(map()) reply to any defined map.
%  SCO message - will assert {this,map,game}_xxx attributes.
%  NOW message - will update entire knowledge base for the new turn.
%  PRN message - outputs debug message 
%  SLO, DRW messages - outputs info messages delaring win or draw.
%  SMR message - prints the end-game summaries.
%  CCD message - do nothing
%
% Bots are encouraged to consult botbase AFTER its own more specific handlers have been
% defined.
%
% There are several *Special* kinds of predicates that hold important attributes. They are:
%   this_xxx        a class of as a bot-specific attributes, such as name, and version.
%     this_name/1 - the name of the bot.
%     this_passcode/1 - the passcode for this session
%     this_turn/2 - the season and year of the current turn
%     this_power/1 - the power the bot is currently playing
%
%   map_xxx         a class of map-specific attributes, such as map_name, province count...
%     map_name/1 - the name of the map.
%
%   game_xxx        a class of game-specific attributes.
%     game_lvl/1 - the diplomacy level of this game (0-140)
%     game_mtl/1 - max seconds available for movement turns
%     game_rtl/1 - max seconds available for retreat turns
%     game_btl/1 - max seconds available for build turns
%     game_dsd/0 - deadline stops on disconnection. If a client
%                  disconnects when it has orders due, the deadline timer stops.
%     game_aoa/0 - any orders accepted.
%

:- multifile handle/1.

:-consult('../daide/daideconn').
:-consult(dipdomain).
% :-consult(dipstanding).

%%%%%% Server-client HANDLERS %%%%%%

% handle/1
%
% handle is this bot's methods of handling type of message.
%

% We don't know ANY maps! we MUST ask for its definition!
% dip_send(msg_dip(yes(map(Map)))) is NOT appropriate.
handle(msg_dip(map(MapName))):-
  assert(map_name(MapName)),
  dip_send_dip(mdf). % we assume we don't know any maps... :(
  
% Note: using home/2 predicate
handle(msg_dip(mdf(Powers,[PScs,Nscs],Adjs))):-
  % START OF MAP PROCESSING
  forall(member(X,Powers),asserta(power(X))),
  % TODO re-instate this...?
  % dip_init_standings(Powers),
  forall(member(centres(Power,Scs),PScs),
    (
      forall(member(S,Scs), (asserta(sc(S)), asserta(prov(S)), asserta(home(Power,S))))
    )
  ),
  retractall(owns(uno,_)),
  forall(member(Z,Nscs),asserta(prov(Z))),
  forall(member(A,Adjs),asserta(A)),
  % END OF MAP PROCESSING
  map_name(MapName),
  dip_send_dip(yes(map(MapName))).


% Note: using this_power/1 predicate
handle(msg_dip(hlo(ThisPower,ThisPasscode,Options))):-
  assert(this_power(ThisPower)),       % the power we're playing as!
  assert(this_passcode(ThisPasscode)), % in the case of a disconnection
  handle_hlo_options(Options).
  
% (ii) Current position  

% Asserts own/2 predicates.
handle(msg_dip(sco(ScoList))):-
  forall(member(sco_entry(Power,Centres),ScoList),
     (
       retractall(owns(Power,_)), asserta(owns(Power,Centres))
     )
  ).
  

% Updates the knowledge base with new information about units.
% All previous position knowledge is discarded.
% TODO add hook to start 'thinking'.
handle(msg_dip(now(turn(Season,Year), Units))):-
  retractall(this_turn/2), assert(this_turn(Season,Year)),
  retractall(unit/3), % retract normal positions
  retractall(unit/4), % retract positions with MRT
  forall(member(X, Units), asserta(X)).

% (iii) Submitting orders

% (iv) Results (useful for collecting statistics?)
% TODO
handle(msg_dip(ord(turn(_Season,_Year),_Order,_Res))):-
  fail.


% Attempt to save the game! (currently rejected.
handle(msg_dip(sve(GameName))):-
  dip_send_dip(rej(sve(GameName))).

% Mal-bracketed client-server message complaint
handle(msg_dip(prn(Msg))):-
  log(warn,'server complained about the following message not having correct parentheses:'),
  log(warn,Msg).

% Civil Disorder
% handle(msg_dip(ccd(Power))).
% Reconnection from Civil Disorder
% handle(msg_dip(not(ccd(Power)))).


% (ix) End of the game
handle(msg_dip(slo(Power))):-
  atom_concat(Power, ' has WON the game!!!', InfoMsg),
  log(info,InfoMsg).

% Server declares a draw.
handle(msg_dip(drw)):-
  log(info,'the server has declared a DRAW!!!').

% End of game summary.
handle(msg_dip(smr(turn(Season,Year),Summaries))):-
  log(info,'********* GAME SUMMARY ********'),
  concat_atom(['Game end: ',Season, ' ',Year], Msg1),
  log(info,Msg1),
  forall(member(psum(Power,Name,Version,Scs), Summaries),
    (
      concat_atom(
        [' ', Power,' played by ', Name, Version,
         ' held ', Scs, ' supply centres.'],
        Msg2
      ),
      log(info,Msg2)
    )
  ),
  forall(member(psum(Power,Name,Version,0,YoE), Summaries),
    (
      concat_atom(
        [' ', Power,' played by ', Name, Version,
         ' was eliminated in ', YoE,'.'],
        Msg3
      ),
      log(info,Msg3)
    )
  ).

handle(msg_dip(rej(X))):-
  term_atom(X,A),
  concat_atom(['sadly, our ',A, ' message got rejected by the server.'],LogMsg),
  log(warn, LogMsg),nl.
  
handle(msg_dip(off)):-
   log(info, 'server requested me to quit. Halting...'), nl,
   halt.

% catch-all for msg_dip
handle(msg_dip(Y)):-
  log(debug,Y), nl, !.

handle(msg_final):-
  log(debug,'received final... halting.'), nl,
  halt.

%%% HANDLERS aux

handle_hlo_options(List):-
  forall(member(Option,List), handle_hlo_option(Option)).
% Playing a level N game!
handle_hlo_option(Op):-
  Op =.. [Name|T],
  atom_concat(game_,Name,NewName),
  NewOp =.. [NewName|T],
  assert(NewOp).


  
% Used to obliterate any Diplomacy-related asserted knowledge, and start anew.
clear_all:-
  clear_game_attr, clear_map_attr, clear_this_attr.

clear_this_attr:-
  retractall(this_name/1), retractall(this_passcode/1), retractall(this_turn/2).

clear_map_attr:-
  retractall(map_name/1).

clear_game_attr:- 
  retractall(game_lvl/1), retractall(game_mtl/1), retractall(game_rtl/1), retractall(game_btl/1),
  retractall(game_dsd/0), retractall(game_aoa/0).
