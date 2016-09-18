-module(app_misc).

-export([
    backup/0,
    cur_dir/0,
    ebins/0,
    get_env/1,
    set_env/2,
    init/0
]).

init() ->
    ok.

cur_dir() ->
    case get_env(cur_dir) of
        undefined ->
            {ok, Dir} = file:get_cwd(),
            D = Dir ++ "/",
            set_env(cur_dir, D),
            D;
        Val -> Val
    end.

ebins() ->
    cur_dir() ++ "ebin/*.beam".

set_env(Key, Val) ->
    mochiglobal:put(Key, Val).

get_env(Key) ->
    mochiglobal:get(Key).

backup() ->
    ok.
