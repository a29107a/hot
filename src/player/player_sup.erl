-module(player_sup).

-behaviour(supervisor).

%% API
-export([
         start_link/0
        ]).

-export([
    start_child/1, 
    stop_all/0,
    all_pid/0, 
    all_id/0
]).

-export([message_queue_len/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-include("server.hrl").

%% ===================================================================
%% API functions
%% ===================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(PlayerId) ->
    ChildSpec = {PlayerId,
                    {player, start_link, [PlayerId]},
                    temporary,
                    16#ffffffff,
                    worker,
                    []
                },
    supervisor:start_child(?MODULE, ChildSpec).


message_queue_len() ->
    case whereis(player_sup) of
        undefined ->
            undefined;
        Pid ->
            {message_queue_len, Len} =
                erlang:process_info(Pid, message_queue_len),
            Len
    end.

all_pid() ->
    [Child || {_Id, Child, _Type, _Modules}
                  <- supervisor:which_children(?MODULE)].

all_id() ->
    [Id || {Id, _Child, _Type, _Modules}
                  <- supervisor:which_children(?MODULE)].

stop_all() ->
    lists:foreach(fun({_, Child, _, _}) ->
        gen_server:call(Child, {stop, ?PLAYER_STOP_NORMAL})        
    end, supervisor:which_children(?MODULE)).
%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    %% for child
    {ok,
     {{one_for_one, 10, 10}, []}
    }.


