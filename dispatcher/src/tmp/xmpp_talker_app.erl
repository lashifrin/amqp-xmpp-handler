-module(xmpp_talker_app).

-behaviour(application).

-export([start/0]).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
	% Start XMPP
	application:start(exmpp),
	% Start our application
    application:start(xmpp_talker),
    ok.

start(_StartType, _StartArgs) ->
    xmpp_talker_sup:start_link().

stop(_State) ->
    ok.
