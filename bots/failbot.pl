% holdbot extends trivialbot:
% 1) It connects to the server, and sends the initial message.
% 2) It will then continually wait for incoming messages, and deal with them in the following
% way:
%   NOW message - holdbot looks for all the units that it 'owns', and submits orders
%                 for all of those units to hold.
%   Delegate all messages to botbase.
% Sends a nme message with the name 'trivialbot', version v0.1.
%

dip_holdbot:-
  dip_connect(localhost,16713),
  dip_send_init,
  repeat,
  dip_receive(Msg),
  handle(Msg),
  fail.

handle(msg_rep(X)):-
  log(debug,X), nl,
  dip_send_dip(nme('Hok\'s FailBot','v0.1')).

% 1) Get the power we're playing as
% 2) Find all of this power's units.
% 3) Attach a hold order to each of those units
% 4) Send these orders.
handle(msg_dip(now(turn(Season,Year), Units))):-
  concat_atom([Season,' ',Year,': FailBot is thinking...'], LogMsg),
  log(info, LogMsg),
  my_units(Units),
  maplist(wrap_hold,Units,Orders),
  dip_send_dip(sub(Orders)).

% wrap_hold/2
%
% Given Term, returns the term with a hold order attached to it.
%
% +Term
% -Wrapped
wrap_hold(Term, hld(Term)).

:-consult(botbase).
