-module(mysql_conf).

-export([
    get/1
]).

get(pool) ->
    [{size, 10}, {max_overflow, 20}];
get(mysql) ->
    [{host, "192.168.1.10"}, {user, "aladdin"}, {password, "sesame"}, {database, "server1"}];
get(Type) ->
    throw({get_mysql_conf_error_type, Type}).
