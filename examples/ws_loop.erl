-module(ws_loop).

-compile(export_all).

ws_versions() ->
    ['draft-hybi-17', 'draft-hybi-10', 'draft-hixie-76', 'draft-hixie-68'].
    

handle_http_eoh(Socket, Method, Path, Headers) ->
    inet:setopts(Socket, [{active, once}, {packet, raw}]),
    case proto_ws:init(ws_versions(), Path, Headers) of
        {ok, Response, WsState} ->
            gen_tcp:send(Response, Socket),
            loop(Socket, WsState);
        _ ->
            exit(ws_only)
    end.

loop(Socket, State) ->
    WsCallback = fun(D, Acc) ->
                         io:format("Received one message: ~p~n", [D]),
                         [D | Acc]
                 end,
    receive
        {tcp, _, Data} ->
            case proto_ws:handle_data(Data, [], WsCallback, State) of
                {Received, websocket_close} ->
                    io:format("Received accumulated messages: ~p~nand closing...~n", [Received]),
                    gen_tcp:close(Socket),
                    {done, normal};
                {Received, websocket_close, CloseData} ->
                    io:format("Received accumulated messages: ~p~nand closing...~n", [Received]),
                    gen_tcp:send(CloseData, Socket),
                    gen_tcp:close(Socket),
                    {done, normal};
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
            gen_tcp:send(proto_ws:format_send(Data, State), Socket),
            loop(Socket, State)
    end.
            
            



















%% Connect and handshake with Websocket.
-spec connect(ServerRef::pid(), Req::#req{}, Ws::#ws{}, WsLoop::function()) -> true.
connect(ServerRef, #req{headers = Headers} = Req, #ws{vsn = Vsn, socket = Socket, socket_mode = SocketMode, path = Path} = Ws, WsLoop) ->
    ?PWS_LOG_DEBUG("building handshake response", []),
    %% get data
    Origin = proto_ws_utility:header_get_value('Origin', Headers),
    Host = proto_ws_utility:header_get_value('Host', Headers),
    %% build handshake
    VsnMod = get_module_name_from_vsn(Vsn),
    HandshakeServer = VsnMod:handshake(Req, Headers, {Path, Origin, Host}),
    %% send handshake back
    misultin_socket:send(Socket, HandshakeServer, SocketMode),
    %% add data to ws record and spawn_link controlling process
    WsT = {misultin_ws, self()},
    WsHandleLoopPid = spawn_link(fun() -> WsLoop(WsT) end),
    %% trap exit
    process_flag(trap_exit, true),
    %% set opts
    misultin_socket:setopts(Socket, [{packet, 0}], SocketMode),
    %% add main websocket pid to misultin server reference
    misultin_server:ws_pid_ref_add(ServerRef, self()),
    %% enter loop
    enter_loop(WsHandleLoopPid, Ws, Req#req.headers, Origin, Host).
