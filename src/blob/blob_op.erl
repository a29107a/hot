-module(blob_op).

-export([
    test1/0,
    test/0,
    upgrade/1,
    pack/1,
    unpack/1
]).

-include("data_type.hrl").

test1() ->
    TimeBegin = now(),
    io:format("begin time ~p~n", [TimeBegin]),
    lists:foreach(fun(_) ->
        test()
    end, lists:seq(1, 10000)),
    TimeEnd = now(),
    io:format("end time ~p~n", [TimeEnd]).

test() ->
    Name = <<"地地中"/utf8>>,
    Player = {player,1,1,binary_to_list(Name),
            [{item,1,1,1},
            {item,1,2,2},
            {item,1,3,3},
            {item,1,4,4},
            {item,1,5,5},
            {item,1,6,6},
            {item,1,7,7},
            {item,1,8,8},
            {item,1,9,9},
            {item,1,10,10}],
            1.33,3},
    %io:format("player ~p~n", [Player]),
    Bin = pack(Player),
    unpack(Bin).
    %io:format("unpack ~p~n", [unpack(Bin)]).

upgrade(In) when is_tuple(In) ->
    TblName = element(1, In),
    TblVersion = element(2, In),
    NameList = atom_to_list(TblName),
    TblAtom = list_to_atom(NameList ++ "_version"),
    TblInfo = TblAtom:version(TblVersion),
    CurVersion = TblAtom:cur_version(),
    CurTblInfo = TblAtom:version(CurVersion),
    {NewData, _} = lists:foldl(fun({Atom, Default, _, _}, {Ret, Count}) ->
        if
            Count == 1 ->
                {erlang:append_element(Ret, Atom), Count + 1};
            Count == 2 ->
                {erlang:append_element(Ret, CurVersion), Count + 1};
            true ->
                {IsOld, Index} = lists:foldl(fun({A, _, _, _}, {F, C}) ->
                    case {F, A == Atom} of
                        {true, _} -> {true, C};
                        {false, true} -> {true, C};
                        {false, false} -> {false, C + 1}
                    end
                end, {false, 1}, TblInfo),
                case IsOld of
                    true -> 
                        {erlang:append_element(Ret, Default), Count + 1};
                    _ -> 
                        {erlang:append_element(Ret, element(Index, TblInfo)), Count + 1}
                end
        end
    end, {{}, 1}, CurTblInfo),
    NewData;
upgrade(_) -> error.    

pack(In) when is_tuple(In) ->
    TblName = element(1, In),
    TblVersion = element(2, In),
    NameList = atom_to_list(TblName),
    TblAtom = list_to_atom(NameList ++ "_version"),
    TblInfo = TblAtom:version(TblVersion),
    %length(TblInfo) == tuple_size(In),
    NameBin = pack_string(NameList),
    inner_pack(<<NameBin/binary, TblVersion:?INT16>>, TblInfo, In, 3);
pack(_) ->
    error.

unpack(In) ->
    {TblName, Rest} = unpack_string(In),
    TblAtom = list_to_atom(TblName ++ "_version"),
    <<Version:?INT16, Rest1/binary>>= Rest,
    TblInfo = TblAtom:version(Version),
    inner_unpack({list_to_atom(TblName), Version},
                    TblInfo, Rest1, 3). 

inner_unpack(Out, TblInfo, Bin, Index) ->
    case length(TblInfo) < Index of
        true -> {Out, Bin};
        false ->
            InTblInfo = lists:nth(Index, TblInfo),
            {Ret, Rest} = unpack_data(Bin, InTblInfo),
            inner_unpack(erlang:append_element(Out, Ret), TblInfo, Rest, Index + 1)
    end.

unpack_data(Bin, {_, _, int, Count}) ->
    case Count of
        1 ->
            <<Ret:?INT8, Rest/binary>> = Bin,
            {Ret, Rest};
        2 ->
            <<Ret:?INT16, Rest/binary>> = Bin,
            {Ret, Rest};
        4 ->
            <<Ret:?INT32, Rest/binary>> = Bin,
            {Ret, Rest};
        8 ->
            <<Ret:?INT64, Rest/binary>> = Bin,
            {Ret, Rest};
        _ -> throw({error_unpack_data_int, Count})
    end;
unpack_data(<<Ret:?FLOAT, Rest/binary>>, {_, _, float}) ->
    {Ret, Rest};
unpack_data(Bin, {_, _, string}) ->
    unpack_string(Bin);
unpack_data(<<Len:?INT16, Rest/binary>>, {_, _, list}) ->
    {Out, Rest2} = lists:foldl(fun(_, {In, Bin}) ->
        {Data, Rest1} = unpack(Bin),
        {[Data | In], Rest1}
    end, {[], Rest}, lists:seq(1, Len)),
    {lists:reverse(Out), Rest2};
unpack_data(_, Type) -> throw({error_type_blob_op, Type}).

inner_pack(Bin, TblInfo, In, Index) ->
    case tuple_size(In) < Index of
        true -> Bin;
        false ->
            InData = element(Index, In),
            InTblInfo = lists:nth(Index, TblInfo),
            PackBin = pack_data(InData, InTblInfo),
            inner_pack(<<Bin/binary, PackBin/binary>>, TblInfo, In, Index + 1)
    end.

pack_data(Data, {_Name, _, int, Count}) ->
    case Count of
        1 -> <<Data:?INT8>>;
        2 -> <<Data:?INT16>>;
        4 -> <<Data:?INT32>>;
        8 -> <<Data:?INT64>>;
        _ -> throw({error_pack_data_int, Count})
    end;
pack_data(Data, {_Name, _, float}) ->
    <<Data:?FLOAT>>;
pack_data(Data, {_Name, _, string}) ->
    pack_string(Data);
pack_data(Data, {_Name, _, list}) ->
    Len = length(Data),
    DataBin = list_to_binary(lists:map(fun pack/1, Data)),
    <<Len:?INT16, DataBin/binary>>;
pack_data(_, Type) -> throw({error_type_blob_op, Type}).

pack_string(Str) when is_list(Str) ->
    Bin = list_to_binary(Str),
    Len = size(Bin),
    case Len > 0 of
        false -> <<0:?INT16>>;
        true ->
            <<Len:?INT16, Bin/binary>>
    end;
pack_string(_) -> <<0:?INT16>>.

unpack_string(Bin) ->
    case Bin of
        <<Len:?INT16, Bin1/binary>> ->
            case Bin1 of
                <<Str:Len/binary-unit:8, Rest/binary>> ->
                    {binary_to_list(Str), Rest};
                _ -> {[], <<>>}
            end;
        _ -> {[], <<>>}
    end.
