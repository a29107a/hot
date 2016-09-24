-module(pack_bin).

-export([
    p16/1,
    p8/1,
    p32/1,
    p64/1,
    pfloat/1,
    u16/1,
    u8/1,
    u32/1,
    u64/1,
    ufloat/1,
    pack_string/1,
    unpack_string/1
]).

-include("data_type.hrl").

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

p16(N) when is_integer(N) ->
    <<N:?INT16>>.

p32(N) when is_integer(N) ->
    <<N:?INT32>>.

p8(N) when is_integer(N) ->
    <<N:?INT8>>.

p64(N) when is_integer(N) ->
    <<N:?INT64>>.

pfloat(F) when is_float(F) ->
    <<F:?FLOAT>>.

u16(<<R:?INT16, Rest/binary>>) ->
    {R, Rest}.

u8(<<R:?INT8, Rest/binary>>) ->
    {R, Rest}.

u32(<<R:?INT32, Rest/binary>>) ->
    {R, Rest}.

u64(<<R:?INT64, Rest/binary>>) ->
    {R, Rest}.

ufloat(<<R:?FLOAT, Rest/binary>>) ->
    {R, Rest}.
