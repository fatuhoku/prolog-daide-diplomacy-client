:-ensure_loaded(library(socket)).
:-ensure_loaded(dppgrammar).
:-ensure_loaded(daideutil).
:-ensure_loaded(dpptokens).

% DAIDE Prolog API
% written by Jacek Lach and Hok Shun Poon
% 
% There are four primitives:
%
% dip_connect
% dip_receive
% dip_send
% dip_disconnect
%

% WARNING: the debug does not print if the statement is blocked somewhere. 
% So to find point of failure put fails/cuts after debug points :^

%%%% CONNECTION %%%%

% dip_connect/0
% convenience method for connecting to a local server.
dip_connect:-
  dip_connect(localhost,16713).

% dip_connect/2
% connects to a DAIDE server with a specified IP address and port.
dip_connect(Host, Port):-
  log(debug,'Connecting to host... \n'),
  tcp_socket(Socket),
  tcp_connect(Socket, Host:Port),
  tcp_open_socket(Socket, In, Out),
  assert(dip_server_in(In)),
  assert(dip_server_out(Out)),
  log(info,'Connected.').

% dip_disconnect/0/2
% Disconnects from the DAIDE server by closing the Input and Output sockets opened.
% Obliged to send a final message just before closing the session.
% 
%
dip_disconnect:-
  log(debug,'Disconnecting... \n'),
  ( dip_server_out(Out); log(warn,'no output stream specified!'), !, fail ),
  ( dip_server_in(In); log(warn,'no input stream specified!'), !, fail ), !,
  dip_disconnect(In,Out).
  
dip_disconnect(In,Out):-
  dip_send(msg_final),
  catch(close(Out),_,log(warn,'failed to close output stream')),
  catch(close(In),_,log(warn,'failed to close input stream')),
  retract(dip_server_out(Out)),
  retract(dip_server_in(In)),
  log(info,'Disconnected.').

%%%% RECEIVING %%%%

% dip_receive/1
%
% Receives from dip_server_out by default.
dip_receive(Msg):-
  dip_server_in(In),
  dip_receive(In,Msg).

% dip_receive/2
%
% Receives a message from the specified inputstream. The specifics of the 
% actual types of messages to be received is delegated to dip_receive_aux functions.
% See the DELEGATED RECEIVE FUNCTIONS section.
% 
% +In the input stream that the Message should be read from.
dip_receive(In, Msg):-
  log(debug,'Waiting for a message...'),
  get_byte(In,Type), get_byte(In,_),
  msg_type_name(Type,Name),
  atom_concat('Found message. Type:\t\t',Name,DbgMsg),
  log(debug,DbgMsg),
  get_byte(In,L1), get_byte(In,L2), bytes_word(L1,L2,Length),
  dip_receive_aux(In, Type, Length, Msg), !. % DELEGATE!


%%%%% DELEGATED RECEIVE FUNCTIONS %%%%%

% dip_receive_aux/4
%
% Receives a message from the specified input stream, given a particular
% message type, and the length (in bytes) of the message.
%
% Message type may be one of
%   Representation Message(RM)
%   Diplomacy Message(DM)
%   Final Message(FM)
%   Error Message(EM).
% When parsed, they are bound to Msg as 
%   msg_rep(Blocks)
%   msg_dip(...)
%   msg_final
%   msg_error(ErrorCode)
% respectively.
% 
% for the format of msg_dip, see dppgrammar.pl for details.
%
% +In the input stream
% +Type the type of the message, an integer.
% -Msg the message.

%%% Msg Type: Representation Message(RM)
% Returns parsed blocks of the representation message.
% Fails if the received message is invalid.
% Refer to Section 3.3 of Client Server Protocol Version 1 Revision 13. 

dip_receive_aux(In, 1, Length, msg_rep(Blocks)):-
  !, dip_receive_rep(In,Length,Blocks).

% Msg Type: Diplomacy Message(DM)
% Returns parsed blocks of the diplomacy message.
% Fails if the received message is invalid.
% Refer to Section 3.4 of Client Server Protocol Version 1 Revision 13. 
%
dip_receive_aux(In, 2, Length, msg_dip(Msg)):-
  !, get_bytes(In, Length, Bytes),
  dip_bytes_tokens(Bytes, Tokens),
  dip_tokens_term(Tokens,Msg).
  

% Msg Type : Final Message (FM)
% The server sends a final message to end the connection. The client is
% obliged to end the connection.
% Refer to section 3.5 of the Client Server Protocol Version 1 Revision 13.
%
dip_receive_aux(_In, 3, 0, msg_final):-
	!, dip_disconnect.

% Msg Type : Error Message (EM)
% The sending party will immediately terminate the connection upon error.
% 
dip_receive_aux(In, 4, 2, msg_error(ErrorMsg)):-	
  !, get_byte(In,E1),get_byte(In,E2),bytes_word(E1,E2,ErrCode),
  dip_code_err(ErrCode, ErrorMsg),
  log(error,ErrorMsg),
  dip_disconnect.
	
% failure case
dip_receive_aux(_In,_Type,_Length,_Msg):-
  log(error,'unknown message type!').

% dip_receive_rep/3
%
% Msg Type: Representation Message(RM)
% Returns a list of parsed representation message blocks. Fails if any of the block reads fail.
% Checks if the length is a multiple of six. if not, fail immediately.
% 
dip_receive_rep(_In, 0, []).
dip_receive_rep(_In, Length, _):-
  \+ 0 is Length mod 6,
  log(error,'invalid representation message length!'), !, fail.
dip_receive_rep(In, N, Blocks):-
  get_bytes(In, N, Bytes),
  dip_rep_bytes_to_blocks(Bytes,Blocks).

% dip_rep_bytes_to_blocks/2
%
% Given a list of bytes, whose length is divisible by 6, convert it into a 
% 
dip_rep_bytes_to_blocks([],[]).
dip_rep_bytes_to_blocks([Category,Id,T1,T2,T3,0|Bytes],[rep_block(Category, Id, Token)|Blocks]):-
  atom_codes(Token,[T1,T2,T3]), dip_rep_bytes_to_blocks(Bytes,Blocks).

%%%% SENDING %%%%
%
% Sends a message to the specified output stream, given a particular
% message type and its data encapsulated.
%
% Message type may be one of
%   Initial Message(IM)
%   Diplomacy Message(DM)
%   Final Message(FM)
%   Error Message(EM).
% When parsed, they are bound to Msg as 
%   msg_initial
%   msg_dip(...)
%   msg_final
%   msg_error(ErrorCode)
% respectively.
% 
% for the format of msg_dip, see dppgrammar.pl for details.
%

% dip_send_header/3
%
% Puts a general message header into the output stream. Note that it
% does NOT flush the stream!
% +Out the output stream
% +Type the type of message
% +Length a 16-bit number representing the length of the message.
dip_send_header(Out, Type, Length):-
  Length >= 0, Length =< 0xFFFF,
  put_byte(Out,Type), put_byte(Out,0),
  word_bytes(Length,Byte1,Byte2),
  put_byte(Out,Byte1), put_byte(Out,Byte2),!.

% dip_send/1
%
% Writes to dip_server_out by default.
dip_send(Msg):-
  dip_server_out(Out),
  dip_send(Out,Msg).

% dip_send/2
%
% Writes out the given message to the output stream.
% +Out the output stream.
% +Msg the message, a Prolog complex term.
%

% Msg Type: Initial Message (IM)
% Refer to Section 3.2 of Client Server Protocol Version 1 Revision 13. 
%
dip_send(Out, msg_initial):-
  dip_send_header(Out,0,4),
  put_byte(Out,0), put_byte(Out,1),
  put_byte(Out,0xDA), put_byte(Out,0x10),
  flush_output(Out), !.

% Msg Type: Diplomacy Message (DM)
dip_send(Out, msg_dip(Msg)):-
  dip_term_tokens(Msg,Tokens),
  dip_bytes_tokens(Bytes,Tokens),
  length(Bytes,Length),
  dip_send_header(Out, 2, Length),
  put_bytes(Out,Bytes),
  flush_output(Out),!,
  log(debug,'  Sent the following tokens...'), log(debug,Tokens), !.

% Msg Type: Final Message (FM)
% Refer to Section 3.5 of Client Server Protocol Version 1 Revision 13. 
dip_send(Out, msg_final):-
  dip_send_header(Out,3,0),
  flush_output(Out), !.

% Msg Type: Error Message (EM)
% -msg_error(Code) Code should correspond to a defined error code
% in accordance to dip_code_err/2.
% Refer to Section 3.6 of Client Server Protocol Version 1 Revision 13. 
dip_send(Out, msg_error(Code)):-
  dip_send_header(Out,4,2),
  word_bytes(Code,B1,B2),
  put_byte(Out,B1), put_byte(Out,B2),
  flush_output(Out), !.

%%% Special short-hand sends %%%
dip_send_init:-         dip_send(msg_initial).
dip_send_dip(DipMsg):-  dip_send(msg_dip(DipMsg)).
dip_send_final:-        dip_send(msg_final).
dip_send_error(Code):-  dip_send(msg_error(Code)).

%%%% TRANSLATION %%%%
% 
% This section details the TRANSLATION between Prolog complex terms (which will be
% referred to as 'Term' in the following section) and the TCP encoding of the 
% DAIDE DPP Message Syntax (which shall be referred to as 'Bytes' in the following section).
%
% DPP language consists of a series of tokens: tokens are categoried, and have a value.
% Structure is introduced by using special bracketing tokens to delimit lists.
%
% Below is a direct mapping of DPP onto Prolog complex terms.
% OBS                        ->                  obs
% NME('aus')('v1.0')         ->    nme([str('name')],[str('v1.0')])
% IAM(RUS)(10)               ->    iam([rus],[10])
% MDF(AUS ENG...)(((RUS WAR)(RUS TRI))
%


% dip_tokens_term/2
% 
% Translates tokens into a Prolog term.
% +Tokens
% -Term
%
dip_tokens_term(Tokens,Term):-
  sc_msg(Term, Tokens,[]),!.
dip_tokens_term(Tokens,_):-
  log(error,'couldn\'t parse server->client diplomacy message. Tokens were:'),
  log(error,Tokens), !, fail.

% dip_term_tokens/2
%
% Translates a Prolog term into tokens.
% +Term the term
% -Tokens the tokens.
%
dip_term_tokens(Term,Tokens):-
  cs_msg(Term, Tokens,[]),!.
dip_term_tokens(Term,_):-
  log(error,'couldn\'t parse client->server diplomacy message. Term was:'),
  log(error,Term), !, fail.

% dip_bytes_tokens/2
%
% Converts a byte-list into a token-list. Because every token is two octets long,
% it must be the case that length(Bytes) is 2*length(Tokens).
% ?Bytes  the bytelist
% ?Tokens the token list
%
dip_bytes_tokens([],[]):-!.
dip_bytes_tokens([Fst,Snd|Bytes], [Token|Tokens]):-
  dip_word_token(Fst, Snd, Token),
  dip_bytes_tokens(Bytes,Tokens).

% dip_word_token/3
%
% Converts a word (2 octets, 16-bits) into a token (a Prolog atom)
% For convenience, the word may be provided as two different Byte parameters.
% First attempt a Code-Category-Value lookup using the DPP definitions, and otherwise
% figure out what kind of token it really is.
%
% +Fst the first byte of the word
% +Snd the second byte of the word
% -Token a Prolog atom representing the token.
%

% See section 4.3 Integers (0x00 - 0x3F)
% 4 cases are
%  2 positive/negative cases
%  x
%  2 reversibility cases
% If positive: i.e. 000xxxxx
% 
dip_word_token(Fst,Snd,Integer):-
  integer(Fst), integer(Snd),
  Fst >= 0, Fst < 0x20, % if positive...
  bytes_word(Fst,Snd,Integer),!.
dip_word_token(Fst,Snd,Integer):-
  integer(Fst), integer(Snd),
  Fst > 0, Fst >= 0x20, Fst =< 0x3F, % if negative...
  bytes_word(Fst,Snd,Int),
  plus(Integer,0x4000,Int),!.
dip_word_token(Fst,Snd,Integer):-
  integer(Integer),
  Integer >= 0, Integer < 0x2000,
  bytes_word(Fst, Snd, Integer),!.
dip_word_token(Fst,Snd,Integer):-
  integer(Integer),
  Integer < 0, Integer >= -0x2000,
  plus(Integer, 0x4000, Int),
  word_bytes(Int, Fst,Snd),!.
  
dip_word_token(0x4B,Code,txt(Char)):-
  char_code(Char,Code),!.
% Token search case
dip_word_token(Fst,Snd,Token):-
  dip_ccv(Token,Fst,Snd),!.
  
% failure case:
dip_word_token(Cat,Code,_Token):-
  ground(Cat), ground(Code),
  concat_atom(['unknown category and/or code: ',Cat,' ',Code],LogMsg),
  log(error,LogMsg), !, fail.
dip_word_token(_Cat,_Code,Token):-
  ground(Token),
  concat_atom(['unrecognized token: ',Token], LogMsg),
  log(error,LogMsg), !,fail.

msg_type_name(0,'Initial Message').
msg_type_name(1,'Representation Message').
msg_type_name(2,'Diplomacy Message').
msg_type_name(3,'Final Message').
msg_type_name(4,'Error Message').
