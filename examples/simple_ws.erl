-module(simple_ws).

-compile(export_all).

ws_versions() ->
    ['draft-hybi-17', 'draft-hybi-10', 'draft-hixie-76', 'draft-hixie-68'].
    

handle_http_eoh(Socket, Method, Path, Headers) ->
    inet:setopts(Socket, [{active, once}, {packet, raw}]),
    case proto_ws:init(ws_versions(), Path, Headers) of
        {true, VsnMod, WsState} ->
            inet:setopts(Socket, [{packet, raw}, {active, once}]),
            {ok, HandshakeResponse} = VsnMod:handshake(Ws),
            gen_tcp:send(HandshakeResponse, Socket),
            loop(Socket, VsnMod, WsState);
        _ ->
            exit(ws_only)
    end.

loop(Socket, VsnMod, WsState) ->
    WsCallback = fun(D, Acc) ->
                         io:format("Received one message: ~p~n", [D]),
                         [D | Acc]
                 end,
    receive
        {tcp, _, Data} ->
            case Mod:handle_data(Data, WsState, [], WsCallback) of
                {Received, websocket_close} ->
                    io:format("Received accumulated messages: ~p~nand closing...~n", [Received]),
                    gen_tcp:close(Socket),
                    {stop, normal};
                {Received, websocket_close, CloseData} ->
                    io:format("Received accumulated messages: ~p~nand closing...~n", [Received]),
                    gen_tcp:send(CloseData, Socket),
                    gen_tcp:close(Socket),
                    {stop, normal};
                {Received, continue, SendData, State2} ->
                    io:format("Received accumulated messages: ~p~n", [Received]),
                    gen_tcp:send(SendData, Socket),
                    inet:setopts(Socket, [{active, once}]),
                    loop(Socket, State2) 
                {Received, continue, State2} ->
                    io:format("Received accumulated messages: ~p~n", [Received]),
                    inet:setopts(Socket, [{active, once}]),
                    loop(Socket, State2) 
            end;
        {send, Data} ->
            gen_tcp:send(VsnMod:format_send(Data), Socket),
            loop(Socket, State)
    end.
            
            
