%%%=========================================================================
%%%                                  META
%%%=========================================================================
%%% @author Joel Ericson <joel.mikael.ericson@gmail.com>
%%%
%%% @copyright Copylefted using some GNU license or other.
%%%
%%% @doc A server that handles debug/error/info messages for modules compiled with {d,debug}
%%%     Output can be redirected into either stdout or files
%%%     In essence, this is a simplified version of error_logger with nicer output.
%%%     
%%%     The callbacks of this module are not to be called directly. Instead, use the %?DEB1/2 ,?DEB2/3 and ?DEBL/3 macros from debug.hrl
%%%
-module(newdebug).
-behaviour(gen_server).
-behaviour(application).

-export([msg/6,timestamp/6]).
-export([
        start_link/0,
        init/0,
        init/1,
        handle_call/3,
        handle_cast/2,
        terminate/2,
        code_change/3,
        handle_info/2
    ]).
% TODO: Create directories/files if nonexistent?

% TODO: Create debug_reporter_N, N ∈ {0..10,err}, where each reporter handles messages for the appropriate level, and the switching is done in the ?DEB_ macros.

-include("../include/debug.hrl").

-define(ERROR_LEVELS,[1,2,3,4,5,6,7,8,9,10,err]).

-export([start/2,stop/1]).

-export([
    input/5
    ,output/1
    ,tty/0
    ,tty/1
    ,debugging/0
    ,debugging/1
    ,status/0
    ,set_level/1
    ,set_level/2
    ,set_trigger_level/1
    ,get_level/0
    ,get_level/1
    ,get_trigger_level/0
    ,add_file/1
    ,block_module/1
    ,unblock_module/1
    ,blocked_modules/0
    ,remove_file/1
    ]).

start_link() ->
    %?DEBL(1,"Starting ~p",[?MODULE]),
    gen_server:start_link({local,?MODULE},?MODULE,[],[]).

start(_,_) ->
    newdebug_sup:start_link().

stop(_) -> ok.

-type int_as_string()::[non_neg_integer()].
    
-record(state,{
        % Whether to output to tty or not
        tty=false::boolean(),
        % If there should be any debugging output
        debugging=true::boolean(),
        % Might use this later on. Statically set to true for now.
        timestamp=true::boolean(),
        % Which levels of debugging are allowed for which modules
        levels=[]::[{module(),int_as_string()}],
        % Global trigger; only show levels below this regardless of specific modules' settings.
        trigger=10,
        % Level for modules with no level specified
        default_level=1::non_neg_integer(),
        % Which output channels we have (excluding tty)
        output=[]::[{Name::any(),Io_device::any()}],
        % Which modules that should never be output from
        blocked_modules=[]::[module()]
    }).

init() ->
    init(kaka).

%set_default_values() ->
%    set_default_values(#state{}).
%
%set_default_values(State) ->
%    set_default_values(State,[debugging,debug_level,default_level,limit]).
%
%set_default_values(State,[]) -> State;
%
%set_default_values(State,[debugging|A]) ->
%    Debugging = case options:get(debug,debugging) of
%        undefined -> false;
%        "false" ->   false;
%        "off" ->     false;
%        _ ->         true
%    end,
%    set_default_values(State#state{debugging=Debugging},A);
%
%set_default_values(State=#state{levels=Levels},[debug_level|A]) ->
%    NewLevels=case options:mget(debug,debug_level) of
%        undefined -> Levels;
%        {list,List} -> List++Levels;
%        {string,List} -> [List|Levels]
%    end,
%    ParsedLevels=lists:map(
%            fun
%                ({String,Thing}) when is_list(String) -> {list_to_atom(String),Thing}; 
%                ({Atom,Thing}) when is_atom(Atom) -> {Atom,Thing} 
%            end, 
%            vol_misc:flip(NewLevels)),
%    set_default_values(State#state{levels=ParsedLevels},A);
%
%
%set_default_values(State=#state{default_level=Level},[default_level|A]) ->
%    DefaultLevel=case options:get(debug,default_level) of
%        undefined -> Level;
%        {ok,Something} -> Something
%    end,
%    set_default_values(State#state{default_level=DefaultLevel},A);
%
%set_default_values(State=#state{trigger=Trigger},[limit|A]) ->
%    TriggerLevel = case options:get(debug,limit) of
%        undefined -> Trigger;
%        {ok,Other} -> Other
%    end,
%    set_default_values(State#state{trigger=TriggerLevel},A).
%

init(_) ->
    process_flag(trap_exit,true),
    lists:foreach(fun newdebug_processor_sup:start_child/1,?ERROR_LEVELS),
    State=#state{},% set_default_values() is deprecated, since we cannot depend on the options server lest rebar go crazy.
    newdebug_processor_sup:set_debugging(State#state.debugging),
    newdebug_processor_sup:set_trigger_level(State#state.trigger),
    newdebug_processor_sup:set_global_level(State#state.default_level),
    {ok,State}.
%    case options:mget(debug,debug_file) of
%        undefined ->
%            {ok,State#state{tty=true}};
%        % If only some of several files has an error, we still get debug output to somewhere.
%        % Thus, we don't really need to stop as long as one of the files succeed.
%        {list,Files} ->
%            {OpenedFiles,Errors}=lists:partition(fun({error,_E}) -> false; (_) -> true end,
%                lists:map(
%                        fun(File) ->
%                                case file:open(File,[append,raw,write]) of
%                                    {ok,FD} ->
%                                        {File,FD};
%                                    _Error ->
%                                        error_logger:format("Couldn't open ~p for logging! (~p)",[File,_Error]),
%                                        {error,File,_Error}
%                                end
%                        end,
%                        Files)),
%            case OpenedFiles of
%                [] ->
%                    {stop,{no_files_opened,Errors}};
%                _ ->
%                    {ok,State#state{output=OpenedFiles}}
%            end;
%        {string,File} ->
%            case file:open(File,[append,write,raw]) of
%                {ok,FD} ->
%                    {ok,State#state{output=[{File,FD}]}};
%                {error,_Error} -> 
%                    error_logger:format("Couldn't open \"~s\" for logging! (~s)",[lists:flatten(File),file:format_error(_Error)]),
%                    {stop,{could_not_open,File,_Error}}
%            end
%    end.

%TODO rehash
% For now, restarting the app suffices.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% @doc Called by the ?DEBL/2/1 macros. Use these instead, and you only need to provide a level, a message and arguments

output(Message) ->
    gen_server:cast(?MODULE,{output,Message}).

input(Level,Module,Line,FormatString,Msg) ->
    gen_server:cast(?MODULE,{input,Level,Module,Line,self(),FormatString,Msg}).

%% @doc Get the internal status of the {@module} server

status() ->
    gen_server:call(?MODULE,status).

%% @doc whether output is written to standard tty output

tty() ->
    gen_server:call(?MODULE,tty).

%% @doc set/unset tty output

tty(Bool) ->
    gen_server:cast(?MODULE,{set_tty,Bool}).

%% @doc Wether anything is output or not.

debugging() ->
    gen_server:call(?MODULE,debugging).

%% @doc Set debugging state.
debugging(Bool) ->
    gen_server:cast(?MODULE,{set_debugging,Bool}).

%% @doc level at or below which output might be generated for all modules with no specific level
%% @see get_level/1

get_level() ->
    gen_server:call(?MODULE,get_default_level).

%% @doc level at or below which output might be generated for all modules with no specific level
get_level(Module) ->
    gen_server:call(?MODULE,{get_level,Module}).

%% @doc level at or below which output is generated 
get_trigger_level() ->
    gen_server:call(?MODULE,get_trigger_level).

%% @doc set level at or below which output might be generated unless otherwise specified
set_level(DefaultLevel) when is_integer(DefaultLevel)->
    newdebug_processor_sup:set_global_level(DefaultLevel).

%% @doc add file to output to
%% @todo only one file supported at the moment.
add_file(FileName) ->
    gen_server:cast(?MODULE,{add_file,FileName}).

%% @doc remove file to output to
remove_file(FileName) ->
    gen_server:cast(?MODULE,{remove_file,FileName}).

%% @doc set level at or below which output might be generated for the specific module
set_level(Level,Module) when is_atom(Module) ->
    set_level(Module,Level); 

set_level(Module,Level) when is_atom(Module) andalso (is_integer(Level) orelse Level==err)->
    gen_server:cast(?MODULE,{set_level,Module,Level}).

%% @doc set level at or below which output is generated 
set_trigger_level(Level) when is_integer(Level)->
    newdebug_processor_sup:set_trigger_level(Level).

%FIXME
block_module(Module) ->
    gen_server:cast(?MODULE,{block_module,Module}).
%FIXME
unblock_module(Module) ->
    gen_server:cast(?MODULE,{unblock_module,Module}).
%FIXME
blocked_modules() ->
    gen_server:call(?MODULE,blocked_modules).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% output utility functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-define(sp(X),case X of {_,err} -> "(Error) "; err -> "(Error) "; A -> ?sc(A) end).
-define(sc(X),string:copies(" ",case X of {_,Y} -> Y; Y -> Y end)).


msg(Level,Module,Line,Self,FormatString,Msg) when is_integer(Level) -> 
    io_lib:format("~-10s:~4..0b ~w ~s"++FormatString++"~n",[Module,Line,Self,?sp(Level)]++Msg);

msg(ListLevel,Module,Line,Self,FormatString,Msg) when is_list(ListLevel) -> 
    Level=list_to_integer(ListLevel),
    io_lib:format("~-10s:~4..0b ~w ~s"++FormatString++"~n",[Module,Line,Self,?sp(Level)]++Msg);

msg({_,Level},Module,Line,Self,FormatString,Msg) when is_integer(Level) -> 
    io_lib:format("~-10s:~4..0b ~w ~s"++FormatString++"~n",[Module,Line,Self,?sp(Level)]++Msg);

msg({_,ListLevel},Module,Line,Self,FormatString,Msg) when is_list(ListLevel) -> 
    Level=list_to_integer(ListLevel),
    io_lib:format("~-10s:~4..0b ~w ~s"++FormatString++"~n",[Module,Line,Self,?sp(Level)]++Msg);

% This is for err and such
msg(Level,Module,Line,Self,FormatString,Msg) ->
    io_lib:format("~-10s:~4..0b ~w ~s"++FormatString++"~n",[Module,Line,Self,?sp(Level)]++Msg).

timestamp(Level,Module,Line,Self,FormatString,Msg) -> 
    Time=tuple_to_list(time()),
    LineInfo=[Module,Line,Self,?sp(Level)],
    LineFormat="\e[33m[~2..0b:~2..0b:~2..0b]\e[32m ~-10s\e[34m~4..0b\e[31m ~w\e[0m ~s\e[0m",
    io_lib:format(LineFormat++long_p(FormatString)++"~n",Time++LineInfo++Msg).

long_p(A) ->
    % Better to reverse when the string is short, and to append to the front.
    long_p(lists:reverse(A),[]).

long_p([],Acc) -> Acc;

long_p([$p,$~|Rest],Acc) ->
    long_p(Rest,"~9999999p"++Acc);

long_p([Other|Rest],Acc) ->
    long_p(Rest,[Other|Acc]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server cast/call/terminate callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Calls

handle_call(get_default_level,_from,State) ->
    {reply,State#state.default_level,State};

handle_call({get_level,Module},_from,State) ->
    case lists:keyfind(Module,1,State#state.levels) of
        {Module,Level} ->
            {reply,Level,State};
        false ->
            {reply,undefined,State}
    end;

                                                              

handle_call(get_trigger_level,_from,State) ->
    {reply,State#state.trigger,State};

handle_call(tty,_From,State) ->
    {reply,State#state.tty,State};

handle_call(debugging,_From,State) ->
    {reply,State#state.debugging,State};

handle_call(status,_From,State) -> 
    {reply,State,State};

handle_call(blocked_modules,_From,State) ->
    {reply,State#state.blocked_modules,State}.



%%% Casts

handle_cast({block_module,Module},State=#state{blocked_modules=Blocked}) ->
    {noreply,State#state{blocked_modules=lists:umerge([Module],lists:sort(Blocked))}};

handle_cast({unblock_module,Module},State=#state{blocked_modules=Blocked}) ->
     NotModule=fun(M) when M == Module -> false;(_) -> true end,
     {noreply,State#state{blocked_modules=lists:filter(NotModule,Blocked)}};

handle_cast({add_file,File},State) ->
    NewState=case lists:keyfind(vol_misc:fix_home(File),1,State#state.output) of
        false ->
            case file:open(File,[append,write,raw]) of
                {ok,FD} ->
                    State#state{output=[{File,FD}]};
                {error,_Error} -> 
                    error_logger:format("Couldn't open \"~s\" for logging! (~s)",[lists:flatten(File),file:format_error(_Error)]),
                    State
            end;
        _ ->
            error_logger:foreach("File \"~s\" already open!",[lists:flatten(File)]),
            State
    end,
    {noreply,NewState};

handle_cast({remove_file,File},State) ->
    NewState=case lists:keysfind(vol_misc:fix_home(File),1,State#state.output) of
        {value,{File,FD},TrimmedState} ->
            case file:close(FD) of
                {error,enospc} -> file:close(FD);
                _ -> ok
            end,
            TrimmedState;
        false ->
            State
    end,
    {noreply,NewState};

handle_cast({set_default_level,DefaultLevel},State) ->
    {noreply,State#state{default_level=DefaultLevel}};

handle_cast({set_trigger_level,Trigger},State) ->
    {noreply,State#state{trigger=Trigger}};

handle_cast({set_level,Module,Level},State=#state{levels=Levels}) ->
    NewLevels=case lists:keytake(Module,1,Levels) of
        {value,_,StrippedLevels} ->
            [{Module,Level}|StrippedLevels];
        false ->
            [{Module,Level}|Levels]
    end,
    newdebug_processor_sup:set_whitelist(NewLevels),
    {noreply,State#state{levels=NewLevels}};

handle_cast({set_tty,Bool},State) when Bool == false ; Bool == true -> 
    {noreply,State#state{tty=Bool}};

handle_cast({set_debugging,Bool},State) when Bool == false ; Bool == true -> 
    {noreply,State#state{debugging=Bool}};

handle_cast({output,Message},State) ->
    lists:foreach(
        fun({_File,FD}) ->
                try file:write(FD,Message) of
                    ok -> ok;
                    {error,Error} ->
                        error_logger:format("Couldn't write to \"~s\"! (~s)\n",[lists:flatten(_File),file:format_error(Error)])
                catch
                    error:Error ->
                        error_logger:format("Couldn't write to \"~s\"!! (~s)\n",[lists:flatten(_File),Error])
                end
        end,
        State#state.output), 
    TTY = State#state.tty,
    if TTY ->
            try io:format(Message) 
            catch
                Type:Error ->
                    error_logger:format("Couldn't write to standard_io! (~w)",[{Type,Error}])
            end,
            {noreply,State};
        true -> 
            {noreply,State}
    end;
    
handle_cast({input,RawLevel,Module,Line,Self,FormatString,Msg},State) -> 
    %?DEBL(3,"Got ~p ~p ~p ~p ~p",[Level,Module,Line,FormatString,Msg]),
    case lists:member(Module,State#state.blocked_modules) of
        true ->
            {noreply,State};
        false ->
            Debugging= State#state.debugging,
            Timestamp= State#state.timestamp,
            Levels = State#state.levels,
            TTY = State#state.tty,
            Level= case RawLevel of
                RawLevel when is_integer(RawLevel) ->
                    integer_to_list(RawLevel);
                RawLevel -> RawLevel
            end,
            {BypassTrigger,CurrentLevel} = if
                Level == err ->
                    {true,"0"};
                true ->
                    % If the level is lower than the default for the module, return it for comparison
                    % Otherwise, return an impossibly high value that always fails to trigger.
                    {Bypass,Comparison}=case lists:keysearch(Module,1,Levels) of
                        {value,{Module,CurLev}} when is_integer(CurLev) -> 
                            {true,integer_to_list(CurLev)};
                        {value,{Module,CurLev}} when is_list(CurLev) -> 
                            {true,CurLev};
                        {value,{Module,CurLev}} ->
                            {true,CurLev};
                        _ -> 
                            {false,State#state.default_level}
                    end,
                    if
                        Comparison < Level ->
                            {false,[ignore]};
                        true ->
                            {Bypass,Level}
                    end
            end,
            TriggerLevel = State#state.trigger,
            %debug:timestamp(1,?MODULE,?LINE,"CurrLevel: ~p, TriggerLevel: ~p",[CurrentLevel,TriggerLevel]);
            %?DEBL(4,"CurrLevel: ~p, TriggerLevel: ~p",[CurrentLevel,TriggerLevel]),
            if
                BypassTrigger orelse CurrentLevel =< TriggerLevel ->
                    %?DEBL(4," ~p =< ~p",[CurrentLevel,TriggerLevel]),
                    if 
                        not Debugging ->
                            {noreply,State};
                        Debugging ->
                            Message=if
                                Level == err ->
                                    timestamp(Level,Module,Line,Self,FormatString,Msg);
                                is_integer(Level) ->
                                    timestamp(Level,Module,Line,Self,FormatString,Msg);
                                is_list(Level) ->
                                    timestamp(list_to_integer(Level),Module,Line,Self,FormatString,Msg);
                                Timestamp ->
                                    timestamp(Level,Module,Line,Self,FormatString,Msg);
                                true ->
                                    msg(Level,Module,Line,Self,FormatString,Msg)
                            end,
                            %?DEBL(5,"Outputting message ~s to ~p",[lists:flatten(Message),State#state.output]),
                            lists:foreach(
                                fun({_File,FD}) ->
                                        try file:write(FD,Message) of
                                            ok -> ok;
                                            {error,Error} ->
                                                error_logger:format("Couldn't write to \"~s\"! (~s)\n",[lists:flatten(_File),file:format_error(Error)])
                                        catch
                                            error:Error ->
                                                error_logger:format("Couldn't write to \"~s\"!! (~s)\n",[lists:flatten(_File),Error])
                                        end
                                end,
                                State#state.output), 
                            if TTY ->
                                    try io:format(Message) 
                                    catch
                                        Type:Error ->
                                            error_logger:format("Couldn't write to standard_io! (~w)",[{Type,Error}])
                                    end,
                                    {noreply,State};
                                true -> 
                                    {noreply,State}
                            end
                    end;
                CurrentLevel > TriggerLevel ->
                    {noreply,State}
            end
    end.
            
terminate(_Reason,_State=#state{output=Files}) ->
    %?DEBL(1,"Terminating ~p",[?MODULE]),
    lists:foreach(
        fun({_File,FD}) ->
                %?DEBL(1,"Closing file ~p",[{File,FD}]),
                case file:close(FD) of
                    {error,enospc} -> file:close(FD);
                    _ -> ok
                end
        end,
        Files).


code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

handle_info(_Info, State) ->
        {noreply, State}.
