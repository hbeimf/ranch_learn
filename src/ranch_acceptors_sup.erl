%% Copyright (c) 2011-2018, Loïc Hoguin <essen@ninenines.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(ranch_acceptors_sup).
-behaviour(supervisor).

-export([start_link/2]).
-export([init/1]).
-include("log.hrl").

-spec start_link(ranch:ref(), module())
	-> {ok, pid()}.
start_link(Ref, Transport) ->
	supervisor:start_link(?MODULE, [Ref, Transport]).

init([Ref, Transport]) ->
	ConnsSup = ranch_server:get_connections_sup(Ref),
	TransOpts = ranch_server:get_transport_options(Ref),
	NumAcceptors = maps:get(num_acceptors, TransOpts, 10),
	Logger = maps:get(logger, TransOpts, error_logger),
	LSocket = case maps:get(socket, TransOpts, undefined) of
		undefined ->
			SocketOpts = maps:get(socket_opts, TransOpts, []),
			%% We temporarily put the logger in the process dictionary
			%% so that it can be used from ranch:filter_options. The
			%% interface as it currently is does not allow passing it
			%% down otherwise.
			put(logger, Logger),
			case Transport:listen(SocketOpts) of
				{ok, Socket} ->
					erase(logger),
					Socket;
				{error, Reason} ->
					listen_error(Ref, Transport, SocketOpts, Reason, Logger)
			end;
		Socket ->
			Socket
	end,
	{ok, Addr} = Transport:sockname(LSocket),
	ranch_server:set_addr(Ref, Addr),
	Procs = [
		{{acceptor, self(), N}, {ranch_acceptor, start_link, [
			LSocket, Transport, Logger, ConnsSup
		]}, permanent, brutal_kill, worker, []}
			|| N <- lists:seq(1, NumAcceptors)],

	?LOG(Procs),

	% [{{acceptor,<0.53.0>,1},
	%   {ranch_acceptor,start_link,[#Port<0.978>,ranch_tcp,error_logger,<0.52.0>]},
	%   permanent,brutal_kill,worker,[]},
	%  {{acceptor,<0.53.0>,2},
	%   {ranch_acceptor,start_link,[#Port<0.978>,ranch_tcp,error_logger,<0.52.0>]},
	%   permanent,brutal_kill,worker,[]},
	%  {{acceptor,<0.53.0>,3},
	%   {ranch_acceptor,start_link,[#Port<0.978>,ranch_tcp,error_logger,<0.52.0>]},
	%   permanent,brutal_kill,worker,[]},
	%  {{acceptor,<0.53.0>,4},
	%   {ranch_acceptor,start_link,[#Port<0.978>,ranch_tcp,error_logger,<0.52.0>]},
	%   permanent,brutal_kill,worker,[]},
	%  {{acceptor,<0.53.0>,5},
	%   {ranch_acceptor,start_link,[#Port<0.978>,ranch_tcp,error_logger,<0.52.0>]},
	%   permanent,brutal_kill,worker,[]},
	%  {{acceptor,<0.53.0>,6},
	%   {ranch_acceptor,start_link,[#Port<0.978>,ranch_tcp,error_logger,<0.52.0>]},
	%   permanent,brutal_kill,worker,[]},
	%  {{acceptor,<0.53.0>,7},
	%   {ranch_acceptor,start_link,[#Port<0.978>,ranch_tcp,error_logger,<0.52.0>]},
	%   permanent,brutal_kill,worker,[]},
	%  {{acceptor,<0.53.0>,8},
	%   {ranch_acceptor,start_link,[#Port<0.978>,ranch_tcp,error_logger,<0.52.0>]},
	%   permanent,brutal_kill,worker,[]},
	%  {{acceptor,<0.53.0>,9},
	%   {ranch_acceptor,start_link,[#Port<0.978>,ranch_tcp,error_logger,<0.52.0>]},
	%   permanent,brutal_kill,worker,[]},
	%  {{acceptor,<0.53.0>,10},
	%   {ranch_acceptor,start_link,[#Port<0.978>,ranch_tcp,error_logger,<0.52.0>]},
	%   permanent,brutal_kill,worker,[]}]

	%% 从打印来看， 这里应该 是启动了 5个监听 进程 ， 
	%% 这之前出来的进程 就是监工{supervisor}， 真正的办工仔 {worker} 就快出现了， ^_^
	%% 现在就先去看下 {ranch_acceptor} 这个接受请求的进程吧，

	{ok, {{one_for_one, 1, 5}, Procs}}.

-spec listen_error(any(), module(), any(), atom(), module()) -> no_return().
listen_error(Ref, Transport, SocketOpts0, Reason, Logger) ->
	SocketOpts1 = [{cert, '...'}|proplists:delete(cert, SocketOpts0)],
	SocketOpts2 = [{key, '...'}|proplists:delete(key, SocketOpts1)],
	SocketOpts = [{cacerts, '...'}|proplists:delete(cacerts, SocketOpts2)],
	ranch:log(error,
		"Failed to start Ranch listener ~p in ~p:listen(~999999p) for reason ~p (~s)~n",
		[Ref, Transport, SocketOpts, Reason, format_error(Reason)], Logger),
	exit({listen_error, Ref, Reason}).

format_error(no_cert) ->
	"no certificate provided; see cert, certfile, sni_fun or sni_hosts options";
format_error(Reason) ->
	inet:format_error(Reason).
