-module(base_attr_version).

-export([
    cur_version/0,
    version/1
]).

cur_version() -> 2.

version(1) ->
    [
    {base_attr, "", string, 0},
    {version, 1, int, 1},
    {exp, 0, int, 8},
    {lv, 0, int, 4},
    {name, "", string, 0}
    ];
version(2) ->
    [
    {base_attr, "", string, 0},
    {version, 2, int, 1},
    {exp, 0, int, 8},
    {lv, 0, int, 4},
    {count, 33, int, 8},
    {name, "", string, 0}
    ];

version(V) ->
    throw({version_error, V}).
