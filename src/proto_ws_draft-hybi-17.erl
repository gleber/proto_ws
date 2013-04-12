%% ==========================================================================================================
%% PROTO_WS based on Misultin - WebSocket - draft hybi 17
%%
%% >-|-|-(°>
%%
%% Copyright (C) 2011, Roberto Ostinelli <roberto@ostinelli.net>.
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
-module('proto_ws_draft-hybi-17').
-behaviour(proto_ws).
-vsn("0.9-dev").

%% API
-export([handshake/1, handshake_continue/4, handle_data/3, format_send/2]).

-export([required_headers/0]).

-include("../include/proto_ws.hrl").

%% macros
-define(HYBI_COMMON, 'proto_ws_draft-hybi-10_17').

%% ============================ \/ API ======================================================================
required_headers() ->
    [
     {'Upgrade', "websocket"}, {'Connection', "Upgrade"}, {'Host', ignore},
     {'Sec-Websocket-Key', ignore}, {'Sec-WebSocket-Version', "13"}
    ].

%% ----------------------------------------------------------------------------------------------------------
%% Description: Callback to build handshake data.
%% ----------------------------------------------------------------------------------------------------------
-spec handshake(wstate()) -> {'ok', binary(), wstate()}.
handshake(State) ->
    ?HYBI_COMMON:handshake(State).

%% ----------------------------------------------------------------------------------------------------------
%% Description: Callback to finalize handshake.
%% ----------------------------------------------------------------------------------------------------------
-spec handshake_continue(WsCallback::fun(),
                         Acc::term(),
                         Data::binary(),
                         State::wstate()) ->
                                {term(), 'websocket_close'} |
                                {term(), 'websocket_close', binary()} |
                                {term(), 'continue', wstate()}  |
                                {term(), 'continue', binary(), wstate()}.
handshake_continue(WsCallback, Acc0, Data, State) ->
    ?HYBI_COMMON:handshake_continue(WsCallback, Acc0, Data, State).

%% ----------------------------------------------------------------------------------------------------------
%% Description: Callback to handle incomed data.
%% ----------------------------------------------------------------------------------------------------------
-spec handle_data(WsCallback::fun(),
                  Acc::term(),
                  State::wstate()) ->
                         {term(), websocket_close} | {term(), websocket_close, binary()} | {term(), continue, wstate()}.
handle_data(WsCallback, Acc0, State) ->
    ?HYBI_COMMON:handle_data(WsCallback, Acc0, State).

%% ----------------------------------------------------------------------------------------------------------
%% Description: Callback to format data before it is sent into the socket.
%% ----------------------------------------------------------------------------------------------------------
-spec format_send(Data::iolist(), State::term()) -> binary().
format_send(Data, State) ->
    ?HYBI_COMMON:format_send(Data, State).

%% ============================ /\ API ======================================================================


%% ============================ \/ INTERNAL FUNCTIONS =======================================================

%% ============================ /\ INTERNAL FUNCTIONS =======================================================
