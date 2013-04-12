%% ==========================================================================================================
%% PROTO_WS based on Misultin - WebSocket
%%
%% >-|-|-(°>
%%
%% Copyright (C) 2011, Roberto Ostinelli <roberto@ostinelli.net>, Joe Armstrong.
%%                     Gleb Peregud <gleber.p@gmail.com> for LivePress Inc.
%% All rights reserved.
%%
%% Code portions from Joe Armstrong have been originally taken under MIT license at the address:
%% <http://armstrongonsoftware.blogspot.com/2009/12/comet-is-dead-long-live-websockets.html>
%%
%% BSD License
%%
%% Redistribution and use in source and binary forms, with or without modification, are permitted provided
%% that the following conditions are met:
%%
%%  * Redistributions of source code must retain the above copyright notice, this list of conditions and the
%%       following disclaimer.
%%  * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and
%%       the following disclaimer in the documentation and/or other materials provided with the distribution.
%%  * Neither the name of the authors nor the names of its contributors may be used to endorse or promote
%%       products derived from this software without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
%% WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
%% PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
%% ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
%% TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
%% HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.
%% ==========================================================================================================
-module('proto_ws_draft-hixie-68').
-behaviour(proto_ws).
-vsn("0.9-dev").

%% API
-export([handshake/1, handshake_continue/4, handle_data/3, format_send/2]).

-export([required_headers/0]).

-include("../include/proto_ws.hrl").

%% ============================ \/ API ======================================================================

required_headers() ->
    [
     {'Upgrade', "WebSocket"}, {'Connection', "Upgrade"}, {'Host', ignore}, {'Origin', ignore}
    ].

%% ----------------------------------------------------------------------------------------------------------
%% Description: Callback to build handshake data.
%% ----------------------------------------------------------------------------------------------------------
-spec handshake(wstate()) -> {'ok', wstate()} | {'ok', binary(), wstate()}.
handshake(#wstate{socket_mode = SocketMode, force_ssl = WsForceSsl, origin = Origin, path = Path, host = Host} = State) ->
    %% prepare handhsake response
    WsMode = case SocketMode of
                 ssl -> "wss";
                 http when WsForceSsl =:= true  -> "wss"; % behind stunnel or similar, client is using ssl
                 http when WsForceSsl =:= false -> "ws"
             end,
    Response = ["HTTP/1.1 101 Web Socket Protocol Handshake\r\n",
                "Upgrade: WebSocket\r\n",
                "Connection: Upgrade\r\n",
                "WebSocket-Origin: ", Origin , "\r\n",
                "WebSocket-Location: ", WsMode, "://", Host, Path, "\r\n\r\n"
               ],
    {ok, Response, State#wstate{inited = true}}.

-spec handshake_continue(WsCallback::fun(),
                         Acc::term(),
                         Data::binary(),
                         State::wstate()) ->
                                {term(), 'websocket_close'} |
                                {term(), 'websocket_close', binary()} |
                                {term(), 'continue', wstate()}  |
                                {term(), 'continue', binary(), wstate()}.
handshake_continue(_CB, _Acc0, _Data, _State) ->
    erlang:error(should_not_happen).

%% ----------------------------------------------------------------------------------------------------------
%% Description: Callback to handle incomed data.
%% ----------------------------------------------------------------------------------------------------------
-spec handle_data(WsCallback::fun(),
                  Acc::term(),
                  State::wstate()) ->
                         {term(), 'websocket_close'} |
                         {term(), 'websocket_close', binary()} |
                         {term(), 'continue', wstate()}  |
                         {term(), 'continue', binary(), wstate()}.
handle_data(CB, Acc0, #wstate{buffer = Buffer} = State) ->
    case i_handle_data(Buffer, <<>>, Acc0, CB) of
        {Acc, continue, Buffer2} ->
            {Acc, continue, State#wstate{buffer = Buffer2}};
        {Acc, websocket_close, SendData} ->
            {Acc, websocket_close, SendData, State#wstate{buffer = <<>>}}
    end.

%% ----------------------------------------------------------------------------------------------------------
%% Description: Callback to format data before it is sent into the socket.
%% ----------------------------------------------------------------------------------------------------------
-spec format_send(Data::iolist(), State::term()) -> iolist().
format_send(Data, _State) ->
    [0, Data, 255].

%% ============================ /\ API ======================================================================


%% ============================ \/ INTERNAL FUNCTIONS =======================================================

%% Buffering and data handling
-spec i_handle_data(Data::binary(),
                    Buffer::binary(),
                    Acc::term(),
                    WsCallback::fun()) ->
                           {term(), websocket_close, SendData::binary()} |
                           {term(), continue, NewBuffer::binary()}.
i_handle_data(Data, Buffer, Acc, WsCallback) ->
    'proto_ws_draft-hixie-76':i_handle_data(Data, Buffer, Acc, WsCallback).

%% ============================ /\ INTERNAL FUNCTIONS =======================================================
