-module(test).
-export([start/0]).

start() -> 
	% 先启动 ranch 依赖的app
	application:start(crypto),
	application:start(asn1),
	application:start(public_key),
	application:start(ssl),
	application:start(ranch),


	{ok, _} = ranch:start_listener(tcp_echo,
		ranch_tcp, [{port, 5678}], test_handler, []),
	ok.
