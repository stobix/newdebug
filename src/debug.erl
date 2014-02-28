%%% @deprecated this is only included as a fallback for newdebug.
-module(debug).

-export([msg/4,msg/5,timestamp/4,timestamp/5]).

% Macros to indent according to level, with errors always being level 0.
-define(sp(X),case X of {_,err} -> "(Error) "; err -> "(Error) "; A -> ?sc(A) end).
-define(sc(X),string:copies(" ",case X of {_,Y} -> Y; Y -> Y end)).
%-define(sp(X)," ").

msg(Level,Module,Line,Msg) ->
    debug_message("~-10s:~4..0b ~w ~s~s~n",[Module,Line,self(),?sp(Level),Msg],Level,Module).

msg(Level,Module,Line,FormatString,Msg) ->
    debug_message("~-10s:~4..0b ~w ~s"++FormatString++"~n",[Module,Line,self(),?sp(Level)]++Msg,Level,Module).

timestamp(Level,Module,Line,Msg) ->
    debug_message("\e[33m[~2..0b:~2..0b:~2..0b]\e[32m ~-10s\e[34m~4..0b\e[31m ~w\e[0m ~s\e[0m~s~n\e[0m",tuple_to_list(time())++[Module,Line,self(),?sp(Level),Msg],Level,Module).

timestamp(Level,Module,Line,FormatString,Msg) ->
    debug_message("\e[33m[~2..0b:~2..0b:~2..0b]\e[32m ~-10s\e[34m~4..0b\e[31m ~w\e[0m ~s\e[0m"++FormatString++"~n",tuple_to_list(time())++[Module,Line,self(),?sp(Level)]++Msg,Level,Module).



-ifdef(modules).
output(FormatString,Message,Token) ->
case ?modules of
  all ->
    io:format(FormatString,Message);
  Modules ->
    case lists:member(Token,Modules) of
      true ->
        io:format(FormatString,Message);
      false -> 
        Token
    end
  end.
-else.
output(_FormatString,_Message,_Token) ->
  no_modules.
-endif.

-ifndef(debug_level).
-define(debug_level,1).
-endif.

-define(checkl(L,Y),
  if L =< ?debug_level -> Y;
    true -> ok
  end).

debug_message(FormatString,Message,Level,Module) ->
  case Level of
    % Show errors regardless of from which module they come.
    {Info,err} -> io:format(standard_error,FormatString,Message);
    % Errors without module info.
    err -> io:format(standard_error,FormatString,Message);
    % Filter normal messages by module.
    {Info,L} -> ?checkl(L,output(FormatString,Message,Info));
    % Messages filtered only by debug level.
    % Obfuscated away, for great justice.
    L -> debug_message(FormatString,Message,{Module,L},Module)
    %?checkl(L,io:format(FormatString,Message))
  end.
      
