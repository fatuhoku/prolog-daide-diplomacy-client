%
% trivialbot
%
% Behavioural Description:
% 
% Delegates all message handling to botbase.
% Mainly used for testing!


dip_trivialbot:-
  dip_connect(localhost,16713),
  dip_send_init,
  repeat,
  dip_receive(Msg),
  handle(Msg),
  fail.
  
:-consult(botbase).