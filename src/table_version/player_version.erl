-module(player_version).

-export([
    cur_version/0,
    version/1
]).

cur_version() -> 2.

version(1) ->
    [{player, "", string, 0},
    {version, 1, int, 2}, 
    {id, 0, int, 8}, 
    {name, "", string, 0}, 
    {items, [], list, 0}, 
    {float_test, 0, float, 0}, 
    {lv, 1, int, 2}];
version(2) ->
    [{player, "", string, 0},
    {version, 2, int, 2}, 
    {id, 0, int, 8}, 
    {name, "", string, 0}, 
    {items, [], list, 0}, 
    {float_test, 0, float, 0},
    {base_attr, {}, record, 0},
    {test, 0, float, 0},
    {lv, 1, int, 2}];

version(V) ->
    throw({version_error, V}).
