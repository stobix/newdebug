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

-include("../include/debug.hrl").

-define(ERROR_LEVELS,[1,2,3,4,5,6,7,8,9,10,err]).

-export([start/2,stop/1]).

-export([
    output/1
    ,tty/0
    ,tty/1
    ,debugging/0
    ,debugging/1
    ,set_level/1
    ,set_level/2
    ,set_trigger_level/1
    ,get_level/0
    ,get_level/1
    ,get_trigger_level/0
    ,add_file/1
    ,remove_file/1
    ,status/0
    ]).

start_link() ->
    gen_server:start_link({local,?MODULE},?MODULE,[],[]).

start(_,_) ->
    newdebug_sup:start_link().

stop(_) -> ok.

status() ->
    gen_server:call(?MODULE,status).

-type statemap()::#{
        % Whether to output to tty or not
        tty=>boolean(),
        % If there should be any debugging output
        debugging=>boolean(),
        % Might use this later on. Statically set to true for now.
        timestamp=>boolean(),
        % Which levels of debugging are allowed for which modules
        levels=>[{module(),non_neg_integer()}],
        % Global trigger; only show levels below this regardless of specific modules' settings.
        trigger=>non_neg_integer(),
        % Level for modules with no level specified
        default_level=>non_neg_integer(),
        % Which output channels we have (excluding tty)
        output=>[{Name::any(),Io_device::any()}]
    }.

init() ->
    init(kaka).

-spec init(any()) -> statemap().

init(_) ->
    process_flag(trap_exit,true),
    lists:foreach(fun newdebug_processor_sup:start_child/1,?ERROR_LEVELS),
    #{debugging:=Debug,trigger:=TLev,default_level:=GLev}
    =State
    =#{
        tty=>false,
        debugging=>true,
        timestamp=>true,
        levels=>[],
        trigger=>10,
        default_level=>10,
        output=>[]

    },% set_default_values() is deprecated, since we cannot depend on the options server lest rebar go crazy.
    newdebug_processor_sup:set_debugging(Debug),
    newdebug_processor_sup:set_trigger_level(TLev),
    newdebug_processor_sup:set_global_level(GLev),
    {ok,State}.

%TODO rehash
% For now, restarting the app suffices.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% @doc Called by the ?DEBL/2/1 macros. Use these instead, and you only need to provide a level, a message and arguments

output(Message) ->
    gen_server:cast(?MODULE,{output,Message}).

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server cast/call/terminate callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Calls

% Needed for R16b1 if I want to know status.
handle_call(status,_from,State) ->
    {reply,State,State};

handle_call(get_default_level,_from,#{default_level:=DLev}=State) ->
    {reply,DLev,State};

handle_call({get_level,Module},_from,#{levels:=Ls}=State) ->
    case lists:keyfind(Module,1,Ls) of
        {Module,Level} ->
            {reply,Level,State};
        false ->
            {reply,undefined,State}
    end;
                                                              

handle_call(get_trigger_level,_from,#{trigger:=T}=State) ->
    {reply,T,State};

handle_call(tty,_From,#{tty:=TTY}=State) ->
    {reply,TTY,State};

handle_call(debugging,_From,#{debugging:=D}=State) ->
    {reply,D,State}.

%%% Casts

handle_cast({add_file,File},#{output:=Output}=State) ->
    NewState=case lists:keyfind(vol_misc:fix_home(File),1,Output) of
        false ->
            io:format("adding file"),
            case file:open(File,[append,write,raw]) of
                {ok,FD} ->
                    State#{output:=[{File,FD}|Output]};
                {error,_Error} -> 
                    error_logger:format("Couldn't open ~9999p for logging! (~s)~n",[File,file:format_error(_Error)]),
                    State
            end;
        _ ->
            error_logger:format("File ~9999p already open!~n",[File]),
            State
    end,
    {noreply,NewState};

handle_cast({remove_file,File},#{output:=Output}=State) ->
    NewState=case lists:keytake(vol_misc:fix_home(File),1,Output) of
        {value,{File,FD},TrimmedOutput} ->
            case file:close(FD) of
                {error,enospc} -> file:close(FD);
                _ -> ok
            end,
            State#{output:=TrimmedOutput};
        false ->
            State
    end,
    {noreply,NewState};

handle_cast({set_default_level,DefaultLevel},State) ->
    newdebug_processor_sup:set_global_level(DefaultLevel),
    {noreply,State#{default_level:=DefaultLevel}};

handle_cast({set_trigger_level,Trigger},State) ->
    newdebug_processor_sup:set_trigger_level(Trigger),
    {noreply,State#{trigger:=Trigger}};

handle_cast({set_level,Module,Level},State=#{levels:=Levels}) ->
    NewLevels=case lists:keytake(Module,1,Levels) of
        {value,_,StrippedLevels} ->
            [{Module,Level}|StrippedLevels];
        false ->
            [{Module,Level}|Levels]
    end,
    newdebug_processor_sup:set_whitelist(NewLevels),
    {noreply,State#{levels:=NewLevels}};

handle_cast({set_tty,Bool},State) when Bool == false ; Bool == true -> 
    {noreply,State#{tty:=Bool}};

handle_cast({set_debugging,Bool},State) when is_boolean(Bool) ->
    newdebug_processor_sup:set_debugging(Bool),
    {noreply,State#{debugging:=Bool}};

handle_cast({output,Message},#{output:=Output,tty:=TTY}=State) ->
    lists:foreach(
        fun({_File,FD}) ->
                try file:write(FD,unicode:characters_to_binary(Message)) of
                    ok -> ok;
                    {error,Error} ->
                      error_logger:format("Couldn't write to \"~tp\"! (~ts). String: ~999999tp\n",[_File,file:format_error(Error),Message])
                catch
                    error:Error ->
                        error_logger:format("Couldn't write to \"~tp\"!! (~tp). String: ~999999tp\n",[_File,Error,Message])
                end
        end,
        Output), 
    if TTY ->
            try io:format(Message) 
            catch
                Type:Error ->
                    error_logger:format("Couldn't write to standard_io! (~w)",[{Type,Error}])
            end,
            {noreply,State};
        true -> 
            {noreply,State}
    end.
    
            
terminate(_Reason,_State=#{output:=Files}) ->
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
