-module(inet_info).

-export([ports/0, sockets/0, info/0, info/1, info/2]).

-type socket() :: {'$inet', module(), any()}.

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

sockets() ->
    gen_tcp_socket:which_sockets() ++ gen_udp_socket:which_sockets().

-spec info() -> list(map()).
info() ->
    [info(Port) || Port <- ports()] ++ [info(Socket) || Socket <- sockets()].

-spec info(S :: port() | socket()) -> map().
info(S) ->
    info(
        S,
        [id, module, recv_oct, send_oct, owner, local_address, foreign_address, state, type]
    ).

-spec info(Port :: port(), Items :: [atom()]) -> map().
info(Port, Items) when is_port(Port), is_list(Items) ->
    port_info(Port, Items, #{});
info(Socket = {'$inet', Module, _}, Items) when is_list(Items) ->
    socket_info(Module, Socket, Items, #{}).

-spec port_info(Port :: port(), Items :: [atom()], Acc :: map()) -> map().
port_info(_Port, [], Acc) ->
    Acc;
port_info(Port, [port | Items], Acc) ->
    case erlang:port_info(Port, id) of
        {id, Id} ->
            port_info(Port, Items, Acc#{id => Id});
        _ ->
            port_info(Port, Items, Acc)
    end;
port_info(Port, [owner | Items], Acc) ->
    case erlang:port_info(Port, connected) of
        {connected, Owner} ->
            port_info(Port, Items, Acc#{owner => Owner});
        _ ->
            port_info(Port, Items, Acc)
    end;
port_info(Port, [local_address | Items], Acc) ->
    unwrap_port_info(local_address, prim_inet:sockname(Port), Port, Items, Acc);
port_info(Port, [foreign_address | Items], Acc) ->
    unwrap_port_info(foreign_address, prim_inet:peername(Port), Port, Items, Acc);
port_info(Port, [state | Items], Acc) ->
    unwrap_port_info(state, prim_inet:getstatus(Port), Port, Items, Acc);
port_info(Port, [type | Items], Acc) ->
    case prim_inet:gettype(Port) of
        {ok, {_, Type}} ->
            port_info(Port, Items, Acc#{type => Type});
        _ ->
            port_info(Port, Items, Acc)
    end;
port_info(Port, [module | Items], Acc) ->
    unwrap_port_info_or_else(module, inet_db:lookup_socket(Port), prim_inet, Port, Items, Acc);
port_info(Port, [Stat | Items], Acc) when
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
            port_info(Port, Items, Acc#{Stat => N});
        _ ->
            port_info(Port, Items, Acc)
    end.

-spec unwrap_port_info(
    Key :: atom(),
    Result :: {ok, term()} | {error, term()},
    Port :: port(),
    Items :: [atom()],
    Acc :: map()
) -> map().
unwrap_port_info(Key, {ok, Value}, Port, Items, Acc) ->
    port_info(Port, Items, Acc#{Key => Value});
unwrap_port_info(_Key, _, Port, Items, Acc) ->
    port_info(Port, Items, Acc).

-spec unwrap_port_info_or_else(
    Key :: atom(),
    Result :: {ok, term()} | {error, term()},
    Default :: term(),
    Port :: port(),
    Items :: [atom()],
    Acc :: map()
) -> map().
unwrap_port_info_or_else(Key, {ok, Value}, _Default, Port, Items, Acc) ->
    port_info(Port, Items, Acc#{Key => Value});
unwrap_port_info_or_else(Key, _, Default, Port, Items, Acc) ->
    port_info(Port, Items, Acc#{Key => Default}).

socket_info(_Socket, _Module, [], Acc) ->
    Acc;
socket_info(Socket, Module, [port | Items], Acc) ->
    case Module:getopts(Socket, [fd]) of
        {ok, [{fd, FD}]} ->
            socket_info(Socket, Module, Items, Acc#{id => FD})
