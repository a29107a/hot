-module(item_version).

-export([
    cur_version/0,
    version/1
]).

cur_version() -> 2.

version(1) ->
    [{item, "", string, 0}, 
    {version, 1, int, 1}, 
    {id, 1, int, 2}, 
    {count, 0, int, 2}];
version(2) ->
    [{item, "", string, 0}, 
    {version, 2, int, 1}, 
    {count, 0, int, 2}];

version(V) ->
    throw({version_error, V}).
