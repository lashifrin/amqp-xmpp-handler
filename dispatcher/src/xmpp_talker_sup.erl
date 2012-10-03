-module(xmpp_talker_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_xmpp_proc/1]).

-include("config.hrl").

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% A startup function for spawning new call FSM.
start_xmpp_proc(Args) ->
    supervisor:start_child(xmpp_talker_sup, []).
%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	{ok, {{one_for_one, 10, 10}, 
		[
		% Starting XMPP Talker
		{xmpp_talker, {xmpp_talker, start_link, [
			?XMPP_TALKER, 
			echo_worker, 
			"leonid@serverteam", % Here goes you JID
			"leonid", % Here is a place for your password
			5222, 
			5000
			]},
			permanent, brutal_kill, worker, [xmpp_talker]}
	]}}.
