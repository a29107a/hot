-module(item_version).

-export([
    cur_version/0,
    version/1
]).

cur_version() -> 1.

version(1) ->
    [{item, string}, {version, int, 1}, {id, int, 2}, {count, int, 2}];

version(V) ->
    throw({version_error, V}).
