-module(player_version).

-export([
    cur_version/0,
    version/1
]).

cur_version() -> 1.

version(1) ->
    [{player, string},
    {version, int, 2}, 
    {id, int, 8}, 
    {name, string}, 
    {items, list}, 
    {float_test, float}, 
    {lv, int, 2}];
version(V) ->
    throw({version_error, V}).
