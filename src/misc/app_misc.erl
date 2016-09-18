-module(app_misc).

-export([
    backup/0,
    cur_dir/0,
    ebins/0,
    init/0
]).

init() ->
    ok.

cur_dir() ->
    {ok, Dir} = file:get_cwd(),
    Dir ++ "/".

ebins() ->
    cur_dir() ++ "ebin/*.beam".

backup() ->
    ok.
