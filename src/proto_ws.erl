%% ==========================================================================================================
%% PROTO_WS based on Misultin - WebSocket
%%
%% >-|-|-(°>
%%
%% Copyright (C) 2011, Roberto Ostinelli <roberto@ostinelli.net>
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
-module(proto_ws).
-vsn("0.9-dev").

%% API
-export([init/2, init/4, init/5, handle_data/4, format_send/2,

         supported_versions/0]).

-include("../include/proto_ws.hrl").

-callback handshake(#wstate{}) -> {'ok', iolist(), #wstate{}}.
-callback handshake_continue(WsCallback::fun(),
                             Acc::term(),
                             Data::binary(),
                             State::wstate()) ->
    {term(), 'websocket_close'} |
    {term(), 'websocket_close', binary()} |
    {term(), 'continue', wstate()}  |
    {term(), 'continue', binary(), wstate()}.
-callback handle_data(WsCallback::fun(),
                      Acc::term(),
                      State::wstate()) ->
    {term(), 'websocket_close'} |
    {term(), 'websocket_close', binary()} |
    {term(), 'continue', wstate()}  |
    {term(), 'continue', binary(), wstate()}.
-callback format_send(Data::iolist(), State::term()) -> binary().

%% behaviour
%% -export([behaviour_info/1]).

%% ============================ \/ API ======================================================================
-spec supported_versions() -> [atom()].
supported_versions() ->
    ['draft-hybi-17', 'draft-hybi-10', 'draft-hixie-76', 'draft-hixie-68'].

-spec init(Path::string(),
           Headers::http_headers()) -> {error, atom()} | {ok, State::#wstate{}} | {ok, Response::binary, State::#wstate{}}.
-spec init(Path::string(),
           SocketMode::socketmode(),
           ForceSsl::boolean(),
           Headers::http_headers()) -> {error, atom()} | {ok, State::#wstate{}} | {ok, Response::binary, State::#wstate{}}.
-spec init(WsVersions::[websocket_version()],
           Path::string(),
           SocketMode::socketmode(),
           ForceSsl::boolean(),
           Headers::http_headers()) -> {error, atom()} | {ok, State::#wstate{}} | {ok, Response::binary, State::#wstate{}}.

init(Path, Headers) ->
    init(proto_ws:supported_versions(), Path, http, false, Headers).
init(Path, SocketMode, ForceSsl, Headers) ->
    init(proto_ws:supported_versions(), Path, SocketMode, ForceSsl, Headers).
init(WsVersions, Path, SocketMode, ForceSsl, Headers) ->
    case check_websockets(WsVersions, Headers) of
        false ->
            {error, ws_not_found};
        {true, Vsn, VsnMod} ->
            Origin = proto_ws_utility:header_get_value('Origin', Headers),
            Host = proto_ws_utility:header_get_value('Host', Headers),
            State = #wstate{vsn = Vsn,
                            vsnmod = VsnMod,
                            headers = Headers,
                            path = Path,
                            origin = Origin,
                            host = Host,
                            socket_mode = SocketMode,
                            force_ssl = ForceSsl},
            case VsnMod:handshake(State) of
                {ok, State2} ->
                    {ok, State2};
                {ok, <<>>, State2} ->
                    {ok, State2};
                {ok, Res, State2} ->
                    {ok, Res, State2}
            end
    end.

format_send(Data, #wstate{vsnmod = VsnMod} = State) ->
    VsnMod:format_send(Data, State).

-spec handle_data(WsCallback::fun(),
                  Acc::term(),
                  Data::binary(),
                  State::wstate()) ->
                         {term(), 'websocket_close'} |
                         {term(), 'websocket_close', binary()} |
                         {term(), 'continue', wstate()}  |
                         {term(), 'continue', binary(), wstate()}.
handle_data(CB, Acc0, Data, #wstate{buffer = Buffer0} = State0) ->
    Buffer = <<Buffer0/binary, Data/binary>>,
    State = State0#wstate{buffer = Buffer},
    case handle_data0(CB, Acc0, State) of
        {Acc, continue, #wstate{buffer = B} = State2} = Ret when B /= Buffer ->
            handle_data(CB, Acc, <<>>, State2);
        Ret ->
            Ret
    end.

handle_data0(CB, Acc0, #wstate{inited = false, vsnmod = VsnMod} = State) ->
    VsnMod:handshake_continue(CB, Acc0, State);
handle_data0(CB, Acc0, #wstate{inited = true, vsnmod = VsnMod} = State) ->
    VsnMod:handle_data(CB, Acc0, State).

%% Check if headers correspond to headers requirements.
-spec check_headers(Headers::http_headers(), RequiredHeaders::http_headers()) -> true | http_headers().
check_headers(Headers, RequiredHeaders) ->
    F = fun({Tag, Val}) ->
                %% see if the required Tag is in the Headers
                case proto_ws_utility:header_get_value(Tag, Headers) of
                    false -> true; % header not found, keep in list
                    HVal ->
                        case Val of
                            ignore -> false; % ignore value -> ok, remove from list
                            HVal -> false;   % expected val -> ok, remove from list
                            Val when is_list(Val) andalso is_list(hd(Val)) ->
                                not(lists:member(HVal, Val));
                            _ ->
                                %% check if header has multiple parameters (for instance FF7 websockets)
                                not(lists:member(Val,string:tokens(HVal,", ")))
                        end
                end
        end,
    case lists:filter(F, RequiredHeaders) of
        [] -> true;
        MissingHeaders -> MissingHeaders
    end.

%% ============================ /\ API ======================================================================


%% ============================ \/ INTERNAL FUNCTIONS =======================================================

%% %% behaviour
%% behaviour_info(callbacks) ->
%%     [
%%      {handshake, 1},
%%      {handshake_continue, 4},
%%      {handle_data, 4},
%%      {format_send, 2}
%%     ];
%% behaviour_info(_) ->
%%     undefined.

%% Loop to check for all available supported websocket protocols.
-spec check_websockets(VsnSupported::[websocket_version()], Headers::http_headers()) -> false | {true, Vsn::websocket_version()}.
check_websockets([], _Headers) -> false;
check_websockets([Vsn|T], Headers) ->
    ?PWS_LOG_DEBUG("testing for websocket protocol ~p", [Vsn]),
    VsnMod = get_module_name_from_vsn(Vsn),
    case check_headers(Headers, VsnMod:required_headers()) of
        true -> {true, Vsn, VsnMod};
        _RemainingHeaders ->
            check_websockets(T, Headers)
    end.

%% convert websocket version to module name
-spec get_module_name_from_vsn(Vsn::websocket_version()) -> atom().
get_module_name_from_vsn(Vsn) ->
    list_to_atom("proto_ws_" ++ atom_to_list(Vsn)).

%% ============================ /\ INTERNAL FUNCTIONS =======================================================
