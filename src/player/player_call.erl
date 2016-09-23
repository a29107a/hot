-module(player_call).

-export([
    handle/3
]).

-include("server.hrl").

handle({stop, Reason}, _, State) ->
    case Reason of
        ?PLAYER_STOP_NORMAL -> ok;
        ?PLAYER_STOP_LOGIN_OTHER ->
            ok;
        ?PLAYER_STOP_KICK_OUT ->
            ok;
        Other ->
            Other
    end,
    {stop, normal, State};

handle(_Msg, _From, State) ->
    {ok, State}.
