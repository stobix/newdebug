%%%-------------------------------------------------------------------
%%% @author stobix
%%% @copyright (C) 2014, stobix
%%% @doc
%%%
%%% @end
%%% Created : 2014-05-30 14:58:42.126154
%%%-------------------------------------------------------------------
%%% This is a special process. A special process is aware of system  
%%% messages and also implements some standard functions needed (and  
%%% standarized) in an OTP environment, so it can be supervised  
%%% (started/stop), upgraded in a hot-upgrade fashion, etc.  
%%%  
%%% See http://www.erlang.org/doc/design_principles/spec_proc.html  
%%% For system messages, see: http://www.erlang.org/doc/man/sys.html  
-module(newdebug_processor_spec).  
%%-------------------------------------------------------------------  
%% API Function Exports  
%%-------------------------------------------------------------------  
-export([start_link/1, init/2]).  
  
%%-------------------------------------------------------------------  
%% Required OTP Exports  
%%-------------------------------------------------------------------  
-export([  
    system_code_change/4, system_continue/3,  
    system_terminate/4, write_debug/3
    ,timestamp/6
]).  
  
%%-------------------------------------------------------------------  
%% API Function Definitions  
%%-------------------------------------------------------------------  
%% @doc Starts a new process synchronously. Spawns the process and  
%%% waits for it to start.  
%% See http://www.erlang.org/doc/man/proc_lib.html  
start_link(N) ->  
    proc_lib:start_link(?MODULE, init, [self(),N]).  
  
%%-------------------------------------------------------------------  
%% API Function Definitions  
%%-------------------------------------------------------------------  
%% @doc Notifies the parent of a successful start and then runs the  
%% main loop. When the process has started, it must call  
%% init_ack(Parent,Ret) or init_ack(Ret), where Parent is the  
%% process that evaluates this function (see start_link/0 above).  
%% At this time, Ret is returned.  
init(Parent,N) ->  
    Name=list_to_atom(lists:concat(["newdebug",N])),
    register(Name, self()),  
    Debug = sys:debug_options([]),  
    proc_lib:init_ack(Parent, {ok, self()}),  
    Level = case N of
        err -> 0;
        _ -> N
    end,
    loop(Parent, Debug, Level, _Debugging=true, _Trigger=0, _Global=0, _Whitelist=[],_Blacklist=[]).
  
%%-------------------------------------------------------------------  
%% Internal Functions  
%%-------------------------------------------------------------------  
%% @doc Our main loop, designed to handle system messages.  
loop(Parent, Debug, Level, Debugging, Trigger, Global, Whitelist,Blacklist) ->
    receive  
        {input,_Module,_Line,_Self,_FormatString,_Msg} when not Debugging ->
            loop(Parent, Debug, Level, Debugging, Trigger, Global, Whitelist,Blacklist);

        {input,Module,Line,Self,FormatString,Msg} when Level =< Global ->
            IsBlack=lists:member(Module,Blacklist),
            if 
                IsBlack -> ok;
                true -> 
                    newdebug:output(timestamp(Level,Module,Line,Self,FormatString,Msg))
            end,
            loop(Parent, Debug, Level, Debugging, Trigger, Global, Whitelist,Blacklist);

        {input,Module,Line,Self,FormatString,Msg} ->
            IsWhite=lists:member(Module,Whitelist),
            if 
                IsWhite -> 
                     newdebug:output(timestamp(Level,Module,Line,Self,FormatString,Msg));
                true -> 
                     ok
            end,
            loop(Parent, Debug, Level, Debugging, Trigger, Global, Whitelist,Blacklist);

        {system, From, get_state} ->
            State = [{level,Level},{debugging,Debugging},{trigger,Trigger},{global,Global},{whitelist,Whitelist},{blacklist,Blacklist}],
            ContState={Level,Debugging,Trigger,Global,Whitelist,Blacklist},
            sys:handle_system_msg(
                get_state, From, Parent, ?MODULE, Debug, {State,ContState} % {State for reply value, State used in system_continue}
            );
        {system, From, {replace_state,StateFun}} ->
            ContState={Level,Debugging,Trigger,Global,Whitelist,Blacklist},
            NewContState=StateFun(ContState),
            sys:handle_system_msg(
                replace_state, From, Parent, ?MODULE, Debug, {NewContState,NewContState}
            );
        {system, From, Request} ->  
            ContState={Level,Debugging,Trigger,Global,Whitelist,Blacklist},
            sys:handle_system_msg(  
                Request, From, Parent, ?MODULE, Debug, ContState
            );  
        {set_global,N} ->
            loop(Parent, Debug, Level, Debugging, Trigger, N, Whitelist,Blacklist);

        {set_whitelist,List} ->
            {WL,BL} =
                lists:foldl(
                    fun
                        ({Module,MLevel},{WAcc,BAcc}) when MLevel < Level ->
                            {WAcc,[Module|BAcc]};
                        ({Module,_MLevel},{WAcc,BAcc}) ->
                            {[Module|WAcc],BAcc}
                    end,
                    {[],[]},
                    List),
            loop(Parent, Debug, Level, Debugging, Trigger, Global, WL,BL);
        {set_debugging,B} ->
            loop(Parent, Debug, Level, B, Trigger, Global, Whitelist,Blacklist);
        {set_trigger,N} ->
            loop(Parent, Debug, Level, Debugging, N, Global, Whitelist,Blacklist);
        Msg ->  
            % Let's print unknown messages.  
            sys:handle_debug(  
                Debug, fun ?MODULE:write_debug/3, ?MODULE, {in, Msg}  
            ),  
            loop(Parent, Debug, Level, Debugging, Trigger, Global, Whitelist,Blacklist)
    end.  
  
%% @doc Called by sys:handle_debug().  
write_debug(Dev, Event, Name) ->  
    io:format(Dev, "~p event = ~p~n", [Name, Event]).  
  
%% @doc http://www.erlang.org/doc/man/sys.html#Mod:system_continue-3  
system_continue(Parent, Debug, {Level,Debugging,Trigger,Global,Whitelist,Blacklist}) ->  
    loop(Parent, Debug, Level, Debugging, Trigger, Global, Whitelist,Blacklist).
  
%% @doc http://www.erlang.org/doc/man/sys.html#Mod:system_terminate-4  
system_terminate(Reason, _Parent, _Debug, _State) ->  
    io:format("Terminate!~n"),  
    exit(Reason).  
  
%% @doc http://www.erlang.org/doc/man/sys.html#Mod:system_code_change-4  
system_code_change(State, _Module, _OldVsn, _Extra) ->  
    io:format("Changed code!~n"),  
    {ok, State}.  

timestamp(Level,Module,Line,Self,FormatString,Msg) -> 
    {Y,Mo,D}=date(),
    {H,M,S}=time(),
    Spaces=case Level of
        0 -> "\e[31;1m (Error) \e[0m";
        _ ->
            string:copies(" ",Level)
    end,
    LineInfo=[Y,Mo,D,H,M,S,Module,Line,Self,Spaces],
    LineFormat="\e[33m[~4..0b-~2..0b-~2..0b ~2..0b:~2..0b:~2..0b]\e[32m ~-10s\e[34m~4..0b\e[31m ~w\e[0m ~s\e[0m",
    io_lib:format(LineFormat++long_p(FormatString)++"~n",LineInfo++Msg).

long_p(A) ->
    % Better to reverse when the string is short, and to append to the front.
    long_p(lists:reverse(A),[]).

long_p([],Acc) -> Acc;

long_p([$p,$~|Rest],Acc) ->
    long_p(Rest,"~9999999p"++Acc);

long_p([Other|Rest],Acc) ->
    long_p(Rest,[Other|Acc]).
