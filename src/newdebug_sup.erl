%%% @hidden
-module(newdebug_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-include("../include/debug.hrl").

start_link() ->
    ?DEBL(1,"Starting ~p",[?MODULE]),
    supervisor:start_link(?MODULE,[]).

init(_) ->
    {ok, {{one_for_one, 3, 10},
            [{newdebug, {newdebug, start_link, []},
                    permanent, 10, worker, [newdebug]}]}}.

