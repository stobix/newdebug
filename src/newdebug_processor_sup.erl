%%%-------------------------------------------------------------------
%%% @author Joel Ericson <joel.mikael.ericson@gmail.com>
%%% @copyright (C) GNU
%%% @doc
%%%
%%% @end
%%% Created : 2014-04-23 16:48:50.872287
%%%-------------------------------------------------------------------
-module(newdebug_processor_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([
    set_global_level_for/2,
    set_debugging_for/2,
    set_trigger_level_for/2,
    set_whitelist_for/2,
    set_global_level/1,
    set_debugging/1,
    set_trigger_level/1,
    set_whitelist/1,
    start_child/1
    ]).

%% Supervisor callbacks
-export([init/1]).


%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
        supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->

        {ok, {{one_for_one,5,4}, []}}.


start_child(N) ->
    supervisor:start_child(newdebug_processor_sup,
        {N,
         {newdebug_processor_spec,start_link,[N]},
         permanent,2000,worker,[newdebug_processor]}).

set_global_level_for(N,M) ->
    list_to_atom(lists:concat(["newdebug",N])) ! {set_global,M}.

set_debugging_for(N,M) ->
    list_to_atom(lists:concat(["newdebug",N])) ! {set_debugging,M}.

set_trigger_level_for(N,M) ->
    list_to_atom(lists:concat(["newdebug",N])) ! {set_trigger,M}.

set_whitelist_for(N,M) ->
    list_to_atom(lists:concat(["newdebug",N])) ! {set_whitelist,M}.

get_children() ->
    lists:map(fun vol_misc:fst/1,supervisor:which_children(?MODULE)).

set_global_level(M) ->
    lists:foreach(fun(N) -> set_global_level_for(N,M) end,get_children()).

set_trigger_level(M) ->
    lists:foreach(fun(N) -> set_trigger_level_for(N,M) end,get_children()).

set_whitelist(M) ->
    lists:foreach(fun(N) -> set_whitelist_for(N,M) end,get_children()).

set_debugging(M) ->
    lists:foreach(fun(N) -> set_debugging_for(N,M) end,get_children()).

