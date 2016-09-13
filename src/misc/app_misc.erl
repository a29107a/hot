-module(app_misc).

-export([
    cur_dir/0,
    init/0
]).

init() ->
    ok.

cur_dir() ->
    {ok, Dir} = file:get_cwd(),
    Dir.
