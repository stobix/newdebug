%%%-------------------------------------------------------------------
%%% @author Joel Ericson <joel.mikael.ericson@gmail.com>
%%% @copyright (C) GNU
%%% @doc
%%%
%%% @end
%%% Created : 2014-04-23 16:19:41.349073
%%%-------------------------------------------------------------------
-module(newdebug_processor).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

 -export([timestamp/6]).

-record(state, {
        level=0,
        debugging=true,
        trigger=0,
        global_level=0,
        whitelist=[]::[{module,level}],
        blacklist=[]::[{module,level}]
    }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(N) ->
        Name=list_to_atom(lists:concat(["newdebug",N])),
        gen_server:start_link({local, Name}, ?MODULE, N, []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec init(err|pos_integer()) -> {ok,#state{}}.
init(err) ->
        {ok, #state{level=0}};

init(N) ->
        {ok, #state{level=N}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
        Reply = ok,
        {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({set_global,N},State) ->
    {noreply,State#state{global_level=N}};

handle_cast({set_whitelist,List},State=#state{level=Level}) ->
    {Whitelist,Blacklist} =
        lists:foldr(
            fun
                ({Module,MLevel},{WAcc,BAcc}) when MLevel < Level ->
                    {WAcc,[Module|BAcc]};
                ({Module,_MLevel},{WAcc,BAcc}) ->
                    {[Module|WAcc],BAcc}
            end,
            {[],[]},
            List),
    {noreply,State#state{whitelist=Whitelist,blacklist=Blacklist}};

handle_cast({set_debugging,B},State) ->
    {noreply,State#state{debugging=B}};

handle_cast({set_trigger,N},State) ->
    {noreply,State#state{trigger=N}};

% When the global level is ≤ current, output unless the module is blacklisted.
handle_cast({input,Module,Line,Self,FormatString,Msg},State=#state{level=Level,trigger=Trigger,debugging=Debugging, global_level=Global,blacklist=Blacklist}) when Debugging andalso Level =< Global -> 
    lists:member(Module,Blacklist) orelse newdebug:output(timestamp(Level,Module,Line,Self,FormatString,Msg)),
    {noreply,State};

% When the global level is > current, only output when the module is whitelisted.   
handle_cast({input,Module,Line,Self,FormatString,Msg},State=#state{level=Level,whitelist=Whitelist,debugging=Debugging}) when Debugging ->
    lists:member(Module,Whitelist) andalso newdebug:output(timestamp(Level,Module,Line,Self,FormatString,Msg)),
    {noreply,State};

handle_cast(_Msg, State) ->
        {noreply, State}.

-define(sp(X),case X of 0 -> "(Error) "; A -> ?sc(A) end).
-define(sc(X),string:copies(" ",case X of {_,Y} -> Y; Y -> Y end)).

timestamp(Level,Module,Line,Self,FormatString,Msg) -> 
    Time=tuple_to_list(time()),
    LineInfo=[Module,Line,Self,?sp(Level)],
    LineFormat="\e[33m[~2..0b:~2..0b:~2..0b]\e[32m ~-10s\e[34m~4..0b\e[31m ~w\e[0m ~ts\e[0m",
    unicode:characters_to_binary(io_lib:format(LineFormat++long_p(FormatString)++"~n",Time++LineInfo++Msg)).

long_p(A) ->
    % Better to reverse when the string is short, and to append to the front.
    long_p(lists:reverse(A),[]).

long_p([],Acc) -> Acc;

long_p([$p,$~|Rest],Acc) ->
    long_p(Rest,"~9999999p"++Acc);

long_p([Other|Rest],Acc) ->
    long_p(Rest,[Other|Acc]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
        {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
        ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================




