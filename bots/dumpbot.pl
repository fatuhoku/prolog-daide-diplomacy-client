% dumpbot extends trivialbot:
% 1) It connects to the server, and sends the initial message.
% 2) It will then continually wait for incoming messages, and deal with them in the following
% way:
%   Sends a nme message with the name 'trivialbot', version v0.1.
%   NOW message - dumpbot calls qsave_program and dumps the entire knowledge base
%                 out into a file, such that it can be consulted by tests.
%   Delegates all other messages to botbase.
%

dip_dumpbot:-
  dip_connect(localhost,16713),
  dip_send_init,
  repeat,
  dip_receive(Msg),
  handle(Msg),
  fail.

handle(msg_rep(X)):-
  log(debug,X), nl,
  dip_send_dip(nme('Hok\'s DumpBot','v0.1')).

% 1) Get the power we're playing as
% 2) Find all of this power's units.
% 3) Attach a hold order to each of those units
% 4) Send these orders.
handle(msg_dip(now(turn(Season,Year), Units))):-
  concat_atom([Season,' ',Year,': DumpBot is dumping...'], LogMsg),
  log(info, LogMsg),
  dip_disconnect,
  halt.

:-consult(botbase).
