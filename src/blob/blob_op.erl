-module(blob_op).

-export([
    check_version/1,
    upgrade/1,
    pack/1,
    unpack/1
]).

-include("data_type.hrl").

check_version(Dir) ->
    {ok, Files} = file:list_dir(Dir),
    lists:foreach(fun(F) ->
        [Name, _] = string:tokens(F, "."),
        Atom = list_to_atom(Name),
        Version = Atom:cur_version(),
        TblInfo = Atom:version(Version),
        lists:foreach(
            fun({_, _, string, _}) ->
                ok;
               ({_, _, int, _}) ->
                ok;
               ({_, _, list, _}) -> 
                ok;
               ({_, _, float, _}) -> 
                ok;
               ({_, _, record, _}) ->
                ok;
               (Type) ->
                throw({check_error, {TblInfo, Type}})
            end, TblInfo)
    end, Files).

upgrade(In) when is_tuple(In) ->
    TblName = element(1, In),
    TblVersion = element(2, In),
    NameList = atom_to_list(TblName),
    TblAtom = list_to_atom(NameList ++ "_version"),
    TblInfo = TblAtom:version(TblVersion),
    CurVersion = TblAtom:cur_version(),
    CurTblInfo = TblAtom:version(CurVersion),
    {NewData, _} = lists:foldl(fun({Atom, Default, Type, _}, {Ret, Count}) ->
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
                    false -> 
                        {erlang:append_element(Ret, Default), Count + 1};
                    _ -> 
                        Data = element(Index, In),
                        NewData = case Type of
                            list ->
                                [upgrade(D) || D <- Data];
                            record ->
                                upgrade(Data);
                            _ -> Data
                        end,
                        {erlang:append_element(Ret, NewData), Count + 1}
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
unpack_data(<<Ret:?FLOAT, Rest/binary>>, {_, _, float, _}) ->
    {Ret, Rest};
unpack_data(Bin, {_, _, string, _}) ->
    unpack_string(Bin);
unpack_data(<<Len:?INT16, Rest/binary>>, {_, _, list, _}) ->
    {Out, Rest2} = lists:foldl(fun(_, {In, Bin}) ->
        {Data, Rest1} = unpack(Bin),
        {[Data | In], Rest1}
    end, {[], Rest}, lists:seq(1, Len)),
    {lists:reverse(Out), Rest2};
unpack_data(<<Rest/binary>>, {_, _, record, _}) ->
    unpack(Rest);
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
pack_data(Data, {_Name, _, float, _}) ->
    <<Data:?FLOAT>>;
pack_data(Data, {_Name, _, string, _}) ->
    pack_string(Data);
pack_data(Data, {_Name, _, list, _}) ->
    Len = length(Data),
    DataBin = list_to_binary(lists:map(fun pack/1, Data)),
    <<Len:?INT16, DataBin/binary>>;
pack_data(Data, {_Name, _, record, _}) ->
    DataBin = pack(Data),
    <<DataBin/binary>>;
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
