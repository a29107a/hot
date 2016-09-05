-module(player_version).

-export([
    cur_version/0,
    version/1
]).

cur_version() -> 1.

version(1) ->
    [{player, "", string},
    {version, 1, int, 2}, 
    {id, 0, int, 8}, 
    {name, "", string}, 
    {items, [], list}, 
    {float_test, 0, float}, 
    {lv, 1, int, 2}];
version(V) ->
    throw({version_error, V}).
