% DAIDE util

% get_bytes/3
%
% Reads Len bytes from the input stream In and unify with the array Bytes
% +In the input stream to read bytes from
% +Len the number of bytes to read
% -Bytes the list of bytes read.
get_bytes(_In,0,[]):-!.
get_bytes(In,Len,[B|T]):-
  get_byte(In,B),
  NewLen is Len-1,
  get_bytes(In,NewLen,T).

% put_bytes/2
%
% Writes out all the bytes in Bytes into the output stream Out.
% +Out  the output stream
% +Bytes the bytes.
put_bytes(_Out,[]):-!.
put_bytes(Out,[Byte|Rest]):-
  put_byte(Out,Byte),
  put_bytes(Out,Rest).


% bytes_word/2
% +Byte1 +Byte2 -Word
% Compiles two 8-bit bytes into a 16-bit word
bytes_word(Byte1, Byte2, Word):-
  Top is Byte1*256,
  plus(Top,Byte2,Word).

% bytes_word/2
% +Word -Byte1 -Byte2
% 16-bit word decompiles into two 8-bit bytes
word_bytes(Word, Byte1, Byte2):-
  Byte1 is Word//256,
  Byte2 is Word rem 256.

%% LOGGING %%
% log_{debug, info, warn, fatal}/2
%
% Prints out a debug message with a level of severity.
% +Severity message
% +Msg message
%
log(debug, Message) :- print('DEBUG: '), print(Message), nl.
log(info,  Message) :- print('INFO : '), print(Message), nl.
log(warn,  Message) :- print('WARN : '), print(Message), nl.
log(error, Message) :- print('ERROR: '), print(Message), nl.
log(fatal, Message) :- print('FATAL: '), print(Message), nl.