-module(main).

-export([
    server_start/0,
    server_stop/0,
    server_stop/1,
    is_running/0, 
    is_running/1,
    reload/0,
    info/0,
    psl/0,
    psl/1,
    psl/2,
    get_info/0
]).

-define(SERVER_APPS,
        [sasl, crypto, inets, ranch, os_mon, hot]).

server_start()->
    ok = application:load(hot),
    app_misc:init(),
    ok = start_applications(?SERVER_APPS).

%% 加载更新
reload() ->
    reloader:reload_all().

%%停止游戏服务器
server_stop() ->
    server_stop(30).

is_running() ->
    is_running(node()).

is_running(Node) ->
    node_misc:is_process_running(Node, server).

%%停止游戏服务器
server_stop(_SleepSeconds) ->
    app_misc:pause_accept(),
    stop_applications(?SERVER_APPS),
    ok.

info() ->
    io:format( "abormal termination:
        ~n   Scheduler id:                         ~p
        ~n   Num scheduler:                        ~p
        ~n   Process count:                        ~p
        ~n   Process limit:                        ~p
        ~n   Memory used by erlang processes:      ~p
        ~n   Memory allocated by erlang processes: ~p
        ~n   The total amount of memory allocated: ~p
        ~n",
        get_info()),
    ok.
get_info() ->
    SchedId      = erlang:system_info(scheduler_id),
    SchedNum     = erlang:system_info(schedulers),
    ProcCount    = erlang:system_info(process_count),
    ProcLimit    = erlang:system_info(process_limit),
    ProcMemUsed  = erlang:memory(processes_used),
    ProcMemAlloc = erlang:memory(processes),
    MemTot       = erlang:memory(total),
    [SchedId, SchedNum, ProcCount, ProcLimit,
     ProcMemUsed, ProcMemAlloc, MemTot].

psl() ->
    psl(100).

psl(Num) ->
    lists:foldl(
      fun(P, true)->
              case erlang:process_info(P, message_queue_len) of
                  {message_queue_len, Count} when Count > Num ->
                      print_process_info(P),
                      false;
                  _ ->
                      true
              end;
         (_, false) ->
              false
      end, true, erlang:processes()).

psl(ProcessPid, Num)    ->
    case erlang:process_info(ProcessPid, message_queue_len) of
        {message_queue_len, Count} when Count > Num ->
            print_process_info(ProcessPid);
        _ ->
            ok
    end.

print_process_info(P) ->
    io:format("~n~n=====process info===~n"
              "~p~n~p~n~p~n~p~n~p~n~p~n~p~n~p~n~p~n~p~n~n~n",
              [P,
               erlang:process_info(P, registered_name),
               erlang:process_info(P, current_function),
               erlang:process_info(P, message_queue_len),
               erlang:process_info(P, status),
               erlang:process_info(P, suspending),
               erlang:process_info(P, last_calls),
               erlang:process_info(P, links),
               erlang:process_info(P, dictionary),
               erlang:process_info(P, current_stacktrace)
              ]).

%%############辅助调用函数##############
manage_applications(Iterate, Do, Undo, SkipError, ErrorTag, Apps) ->
    Iterate(fun(App, Acc) ->
                    case Do(App) of
                        ok -> [App | Acc];%合拢
                        {error, {SkipError, _}} when is_atom(SkipError) ->
                            Acc;
                        {error, {Error, Reason}} when is_list(SkipError) ->
                            case lists:member(Error, SkipError) of
                                true ->
                                    Acc;
                                false ->
                                    io:format(
                                       "App ~p, Reason ~p~n", [App, Reason]),
                                    lists:foreach(Undo, Acc),
                                    throw({error, {ErrorTag, App, Reason}})
                            end;
                        {error, Reason} ->
                            io:format("App ~p, Reason ~p~n", [App, Reason]),
                            lists:foreach(Undo, Acc),
                            throw({error, {ErrorTag, App, Reason}})
                    end
            end, [], Apps),
    ok.

start_applications(Apps) ->
    manage_applications(fun lists:foldl/3,
                        fun application:start/1,
                        fun application:stop/1,
                        [already_started, cannot_start_application],
                        cannot_start_application,
                        Apps).

stop_applications(Apps) ->
    io:format("stop_applications stopping.~n",[]),
    manage_applications(fun lists:foldr/3,
                        fun application:stop/1,
                        fun application:start/1,
                        not_started,
                        cannot_stop_application,
                        Apps).


