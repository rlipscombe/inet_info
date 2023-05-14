-module(inet_info).

-export([ports/0, info/0, info/1, info/2]).

ports() ->
    lists:filter(
        fun(Port) ->
            case erlang:port_info(Port, name) of
                {name, Name} when
                    Name =:= "tcp_inet";
                    Name =:= "udp_inet";
                    Name =:= "sctp_inet"
                ->
                    true;
                _ ->
                    false
            end
        end,
        erlang:ports()
    ).

-spec info() -> list(map()).
info() ->
    [info(Port) || Port <- ports()].

-spec info(Port :: port()) -> map().
info(Port) when is_port(Port) ->
    info(
        Port,
        [port, module, recv_oct, send_oct, owner, local_address, foreign_address, state, type]
    ).

-spec info(Port :: port(), Items :: [atom()]) -> map().
info(Port, Items) when is_port(Port), is_list(Items) ->
    info(Port, Items, #{}).

-spec info(Port :: port(), Items :: [atom()], Acc :: map()) -> map().
info(_Port, [], Acc) ->
    Acc;
info(Port, [port | Items], Acc) ->
    case erlang:port_info(Port, id) of
        {id, Id} ->
            info(Port, Items, Acc#{id => Id});
        _ ->
            info(Port, Items, Acc)
    end;
info(Port, [owner | Items], Acc) ->
    case erlang:port_info(Port, connected) of
        {connected, Owner} ->
            info(Port, Items, Acc#{owner => Owner});
        _ ->
            info(Port, Items, Acc)
    end;
info(Port, [local_address | Items], Acc) ->
    unwrap_info(local_address, prim_inet:sockname(Port), Port, Items, Acc);
info(Port, [foreign_address | Items], Acc) ->
    unwrap_info(foreign_address, prim_inet:peername(Port), Port, Items, Acc);
info(Port, [state | Items], Acc) ->
    unwrap_info(state, prim_inet:getstatus(Port), Port, Items, Acc);
info(Port, [type | Items], Acc) ->
    case prim_inet:gettype(Port) of
        {ok, {_, Type}} ->
            info(Port, Items, Acc#{type => Type});
        _ ->
            info(Port, Items, Acc)
    end;
info(Port, [module | Items], Acc) ->
    unwrap_info_or_else(module, inet_db:lookup_socket(Port), prim_inet, Port, Items, Acc);
info(Port, [Stat | Items], Acc) when
    Stat =:= recv_cnt;
    Stat =:= recv_max;
    Stat =:= recv_avg;
    Stat =:= recv_dvi;
    Stat =:= send_cnt;
    Stat =:= send_avg;
    Stat =:= send_pend;
    Stat =:= send_oct;
    Stat =:= recv_oct
->
    case prim_inet:getstat(Port, [Stat]) of
        {ok, [{Stat, N}]} ->
            info(Port, Items, Acc#{Stat => N});
        _ ->
            info(Port, Items, Acc)
    end.

-spec unwrap_info(
    Key :: atom(),
    Result :: {ok, term()} | {error, term()},
    Port :: port(),
    Items :: [atom()],
    Acc :: map()
) -> map().
unwrap_info(Key, {ok, Value}, Port, Items, Acc) ->
    info(Port, Items, Acc#{Key => Value});
unwrap_info(_Key, _, Port, Items, Acc) ->
    info(Port, Items, Acc).

-spec unwrap_info_or_else(
    Key :: atom(),
    Result :: {ok, term()} | {error, term()},
    Default :: term(),
    Port :: port(),
    Items :: [atom()],
    Acc :: map()
) -> map().
unwrap_info_or_else(Key, {ok, Value}, _Default, Port, Items, Acc) ->
    info(Port, Items, Acc#{Key => Value});
unwrap_info_or_else(Key, _, Default, Port, Items, Acc) ->
    info(Port, Items, Acc#{Key => Default}).
