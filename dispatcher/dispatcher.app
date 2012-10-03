%% -*- mode: Erlang; -*-

{application, dispatcher,
 [
  {description, "Dispatcher"},
  {vsn, "1.0"},
  {id, "dispatcher"},
  {modules, [
			 dispatcher,
			 dispatcher_app,
			 dispatcher_broker,
			 dispatcher_ep,
			 xmpp_talker_sup,
			 xmpp_talker,
			 echo_worker,
                         test_worker
			]},
  {registered, [
				dispatcher_broker_sup,
				dispatcher_ep_sup,
				xmpp_dummy_ep_sup
			   ]},
  {applications, [kernel, stdlib]},
  {mod, {dispatcher_app, []}},
  {env, []}
 ]
}.
