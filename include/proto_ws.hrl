%% ==========================================================================================================
%% PROTO_WS based on Misultin - Include file
%%
%% Copyright (C) 2011, Sean Hinde, Roberto Ostinelli <roberto@ostinelli.net>
%% All rights reserved.
%%
%% BSD License
%%
%% Redistribution and use in source and binary forms, with or without modification, are permitted provided
%% that the following conditions are met:
%%
%%  * Redistributions of source code must retain the above copyright notice, this list of conditions and the
%%    following disclaimer.
%%  * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and
%%    the following disclaimer in the documentation and/or other materials provided with the distribution.
%%  * Neither the name of the authors nor the names of its contributors may be used to endorse or promote
%%    products derived from this software without specific prior written permission.
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


%% ============================ \/ LOG ======================================================================
-ifdef(log_debug).
-define(LOG_DEBUG(Str, Args), erlang:apply(error_logger, info_msg, [lists:concat(["[DEBUG]      pid: ", pid_to_list(self()), "~n        module: ", ?MODULE, "~n line: ", ?LINE, "~n", Str, "~n"]), Args])).
-define(LOG_INFO(Str, Args), erlang:apply(error_logger, info_msg, [lists:concat(["      module: ", ?MODULE, "~n line: ", ?LINE, "~n", Str, "~n"]), Args])).
-define(LOG_WARNING(Str, Args), erlang:apply(error_logger, warning_msg, [lists:concat(["        module: ", ?MODULE, "~n line: ", ?LINE, "~n", Str, "~n"]), Args])).
-define(LOG_ERROR(Str, Args), erlang:apply(error_logger, error_msg, [lists:concat(["    module: ", ?MODULE, "~n line: ", ?LINE, "~n", Str, "~n"]), Args])).
-else.
-ifdef(log_info).
-define(LOG_DEBUG(Str, Args), ok).
-define(LOG_INFO(Str, Args), erlang:apply(error_logger, info_msg, [lists:concat(["      module: ", ?MODULE, "~n line: ", ?LINE, "~n", Str, "~n"]), Args])).
-define(LOG_WARNING(Str, Args), erlang:apply(error_logger, warning_msg, [lists:concat(["        module: ", ?MODULE, "~n line: ", ?LINE, "~n", Str, "~n"]), Args])).
-define(LOG_ERROR(Str, Args), erlang:apply(error_logger, error_msg, [lists:concat(["    module: ", ?MODULE, "~n line: ", ?LINE, "~n", Str, "~n"]), Args])).
-else.
-ifdef(log_error).
-define(LOG_DEBUG(Str, Args), ok).
-define(LOG_INFO(Str, Args), ok).
-define(LOG_WARNING(Str, Args), ok).
-define(LOG_ERROR(Str, Args), erlang:apply(error_logger, error_msg, [lists:concat(["    module: ", ?MODULE, "~n line: ", ?LINE, "~n", Str, "~n"]), Args])).
-else.
%% default to warning level
-define(LOG_DEBUG(Str, Args), ok).
-define(LOG_INFO(Str, Args), ok).
-define(LOG_WARNING(Str, Args), erlang:apply(error_logger, warning_msg, [lists:concat(["        module: ", ?MODULE, "~n line: ", ?LINE, "~n", Str, "~n"]), Args])).
-define(LOG_ERROR(Str, Args), erlang:apply(error_logger, error_msg, [lists:concat(["    module: ", ?MODULE, "~n line: ", ?LINE, "~n", Str, "~n"]), Args])).
-endif.
-endif.
-endif.
%% ============================ /\ LOG ======================================================================


%% ---------------------------- \/ HTTP ---------------------------------------------------------------------
-type http_version() :: {Maj::non_neg_integer(), Min::non_neg_integer()}.

-type http_header() :: 'Cache-Control' | 'Connection' | 'Date' | 'Pragma' | 'Transfer-Encoding' | 'Upgrade' |
                       'Via' | 'Accept' | 'Accept-Charset' | 'Accept-Encoding' | 'Accept-Language' |
                       'Authorization' | 'From' | 'Host' | 'If-Modified-Since' | 'If-Match' |
                       'If-None-Match' | 'If-Range' | 'If-Unmodified-Since' | 'Max-Forwards' |
                       'Proxy-Authorization' | 'Range' | 'Referer' | 'User-Agent' | 'Age' | 'Location' |
                       'Proxy-Authenticate' | 'Public' | 'Retry-After' | 'Server' | 'Vary' | 'Warning' |
                       'Www-Authenticate' | 'Allow' | 'Content-Base' | 'Content-Encoding' |
                       'Content-Language' | 'Content-Length' | 'Content-Location' | 'Content-Md5' |
                       'Content-Range' | 'Content-Type' | 'Etag' | 'Expires' | 'Last-Modified' |
                       'Accept-Ranges' | 'Set-Cookie' | 'Set-Cookie2' | 'X-Forwarded-For' | 'Cookie' |
                       'Keep-Alive' | 'Proxy-Connection' | list() | binary().

-type http_headers() :: list({http_header(), list() | binary() | integer() | atom()}).

-type http_method() :: 'GET' | 'POST' | 'HEAD' | 'PUT' | 'DELETE' | 'TRACE' | 'CONNECT'.

-type http_connection() :: close | keep_alive.

-type http_uri() ::
        {abs_path, Path::list()} |
        {absoluteURI, Path::list()} |
        {absoluteURI, http | https | atom(), Host::binary(), Port::non_neg_integer(), Path::list()} |
        {scheme, Scheme::list(), RequestString::list()}.

-type http_supported_encoding() :: deflate | gzip.
%% ---------------------------- /\ HTTP ---------------------------------------------------------------------

%% ---------------------------- \/ OTHER --------------------------------------------------------------------
-type websocket_version() ::
        'draft-hybi-10' |
        'draft-hybi-17' |
        'draft-hixie-68' |
        'draft-hixie-76'.

-type socketmode() :: http | ssl.

-type gen_proplist() :: [{Tag::atom()|list()|binary(), Value::term()}].
-type gen_proplist_options() :: [{Tag::atom()|list()|binary(), Value::term()} | atom()].

-type date_tuple() :: {{non_neg_integer(), 1..12, 1..31}, {0..24, 0..60, 0..60}}.

%% ---------------------------- /\ OTHER --------------------------------------------------------------------

%% ============================ /\ TYPES ====================================================================

-record(wstate, {vsn,
                 vsnmod,

                 path,
                 origin,
                 host,

                 socket_mode,
                 force_ssl,

                 headers,
                 inited = false,

                 buffer = <<>>,
                 internal}).

-type wstate() :: #wstate{}.

%%% ============================ /\ RECORDS ==================================================================
