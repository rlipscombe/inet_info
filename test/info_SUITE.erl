-module(info_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [listening_socket].

listening_socket(_Config) ->
    {ok, Socket} = gen_tcp:listen(0, []),
    {ok, {_, Port}} = inet:sockname(Socket),
    Owner = self(),
    ?assertMatch(
        [
            #{
                local_address := {{0, 0, 0, 0}, Port},
                module := inet_tcp,
                owner := Owner,
                state := [listen, open],
                type := stream
            }
        ],
        inet_info:info()
    ).
