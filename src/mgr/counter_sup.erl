-module(counter_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-export([
    add_all_server/1,
    add_server/1
]).

-include("counter.hrl").

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
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    PlayerIdCounter = {
        ?PLAYERID_COUNTER,
        {
            counter_mgr,
            start_link,
            [?PLAYERID_COUNTER]
        },
        Restart,
        Shutdown,
        Type,
        []
    },

    GuildIdCounter = {
        ?GUILDID_COUNTER,
        {
            counter_mgr,
            start_link,
            [?GUILDID_COUNTER]
        },
        Restart,
        Shutdown,
        Type,
        []
    },


    {
        ok, 
        {
            SupFlags, 
            [
                GuildIdCounter,
                PlayerIdCounter
            ]
        }
    }.

%%%===================================================================
%%% Internal functions
%%%===================================================================
add_all_server(Type) ->
    lists:map(fun(Node) ->
        rpc:call(Node, counter_sup, add_server, [Type])
    end, [node() | nodes()]).

add_server(Type) ->
    case whereis(Type) of
        undefined ->
            Restart = permanent,
            Shutdown = 2000,
            Type = worker,
            Child = {
                Type,
                {
                    counter_mgr,
                    start_link,
                    [Type]
                },
                Restart,
                Shutdown,
                Type,
                []
            },
            supervisor:start_child(?MODULE, Child);
        _ -> had_started           
    end.
