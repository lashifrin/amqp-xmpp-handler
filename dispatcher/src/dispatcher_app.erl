-module(dispatcher_app).
-author('Maxim Treskin').

-behaviour(application).

-include("dispatcher.hrl").
-include("config.hrl").

%% Internal API
-export([
         start_ep_proc/1,
         start_xmpp_proc/1,
         start_dummy_proc/1,
         get_app_env/2
        ]).

%% Application and Supervisor callbacks
-export([start/2, stop/1, init/1]).

-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).

%% A startup function for spawning new call FSM.
start_ep_proc(Args) ->
    ?INFO("----> dispatcher:start_ep_proc <------", []),
    supervisor:start_child(dispatcher_ep_sup, [Args]).

%% A startup function for spawning new call FSM.
%% This function did not work from the start - requires review
%% 10.01.2012
start_xmpp_proc(Args) ->
    supervisor:start_child(xmpp_talker_sup, [Args]).

%% A startup function for spawning new call GEN_SERVER.
%% This one added by me to verify how to add a child process
%% 10.01.2012
start_dummy_proc(Args) ->
    supervisor:start_child(xmpp_dummy_ep_sup, [Args]).

%%----------------------------------------------------------------------
%% Application behaviour callbacks
%%----------------------------------------------------------------------
start(Application, Type) ->
    ?INFO("Starting: ~p ~p", [Application, Type]),
    ?INFO("Starting Exmpp: ~n", []),
    supervisor:start_link({local, ?MODULE}, ?MODULE, [tt]).

stop(_S) ->
    ok.

%%----------------------------------------------------------------------
%% Supervisor behaviour callbacks
%%----------------------------------------------------------------------

init([xmpp_dummy_ep]) ->
    ?INFO("----> init(xmpp_dummy_ep) <------", []),
    {ok,
     {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
      [
       %% EP Client
       { undefined, {test_worker, start_link, []},
         temporary, 2000, worker, [] }
      ]
     }
    };
init([dispatcher_ep]) ->
    ?INFO("----> init(dispatcher_ep) <------", []),
    {ok,
     {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
      [
       %% EP Client
       { undefined, {dispatcher_ep, start_link, []},
         temporary, 2000, worker, [] }
      ]
     }
    };
init([xmpp_talker]) ->
    ?INFO("----> init(xmpp_talker) <------", []),
    {ok,
     {_SupFlags = {one_for_one, 10, 10},
      [
       %% Starting XMPP talker
       { undefined, {xmpp_talker, start_link, [

%%                        ?XMPP_TALKER,
%%                        echo_worker,
%%                        "leonid@serverteam", % Here goes you JID
%%%                        "leonid", % Here is a place for your password
%%                        5222,
%%                        5000

	]},
        permanent, brutal_kill, worker, [] }
      ]
     }
    };
init([tt]) ->
    ?INFO("----> init(tt) <------", []),
    {ok,
     {_SupFlags = {one_for_one, ?MAX_RESTART, ?MAX_TIME},
      [
       %% Broker server
       { dispatcher_broker_sup, 
           {dispatcher_broker, start_link, []},
             permanent, 
             2000, 
             worker, 
             [dispatcher_broker] 
       },
       %% EP instance supervisor
       { dispatcher_ep_sup, 
           {supervisor,start_link,
                      [{local, dispatcher_ep_sup}, ?MODULE, [dispatcher_ep]]},
            permanent, 
            infinity, 
            supervisor, 
            [] 
       },
       %% Dummy instance supervisor
       { xmpp_dummy_ep_sup, 
            {supervisor,start_link,
                      [{local, xmpp_dummy_ep_sup}, ?MODULE, [xmpp_dummy_ep]]},
          permanent, 
          infinity, 
          supervisor, 
          [] 
        },
       %% EP instance supervisor
       { dispatcher_ep_sup2, 
           {supervisor,start_link,
                      [{local, dispatcher_ep_sup2}, ?MODULE, [dispatcher_ep]]},
            permanent, 
            infinity, 
            supervisor, 
            [] 
       }
      ]
     }
   }.

%%----------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------
get_app_env(Opt, Default) ->
    case application:get_env(Opt) of
        {ok, Val} -> Val;
        _ -> Default
    end.
%,
%       %% XMPP talker instance supervisor
%       { xmpp_talker_sup, {supervisor,start_link,
%                      [{local, xmpp_talker_sup},
%%%                       ?MODULE, [xmpp_talker]]},
%                       ?MODULE, [xmpp_dummy_ep]]},
%         permanent, infinity, supervisor, [] }
%
%      ]
       %% Dummy instance supervisor
%       { xmpp_dummy_ep_sup, {supervisor,start_link,
%                      [{local, xmpp_dummy_ep_sup},
%                       ?MODULE, [xmpp_dummy_ep]]},
%         permanent, infinity, supervisor, [] } ] }
