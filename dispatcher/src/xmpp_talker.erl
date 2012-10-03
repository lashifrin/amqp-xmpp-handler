%%%---------------------------------------------------------------------------------------
%%% File    : xmpp_talker.erl
%%% Author  : Max Burinov <bourinov@gmail.com>
%%% Description : Universal XMPP client
%%%
%%% It sits and waits for incoming messages. When it gets one, it creates a new
%%% worker process and gives the message's from-jid and its body for processing.
%%%
%%% When the job in the worker process is done it casts back a jid-to and a body2, 
%%% to xmpp_talker. xmpp_talker sends a new message to-jid with body2. This is it.
%%%
%%% In case connection interruption it also tries to re-connect.
%%%---------------------------------------------------------------------------------------
%%% Created by Max Burinov on 18.10.2011.
%%% Copyright (c) 1979-3188 Max Burinov. All Rights Reserved.
%%%
%%% This file is provided to you under the Apache License, Version 2.0 (the "License"); 
%%% you may not use this file except in compliance with the License.  You may obtain
%%% a copy of the License at
%%%
%%%   http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software distributed under 
%%% the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF 
%%% ANY KIND, either express or implied.  See the License for the specific language 
%%% governing permissions and limitations under the License.
%%%---------------------------------------------------------------------------------------
-module(xmpp_talker).

-behaviour(gen_server).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").

%% API
-export([start_link/6]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
		session,	% XMPP session
		worker,		% worker ::atom()
		wait,		% time xmpp_talker have to wait until self-restart
		jid,		% xmpp_talker JID
		port, 		% port
		password	% password
	}).

-define(SERVER, ?MODULE).

%%========================================================================================
%% API
%%========================================================================================
%%----------------------------------------------------------------------------------------
%% Description: Starts the server
%% Function: start_link(Name) -> {ok,Pid} | ignore | {error,Error}
%%              Name :: atom() - name of the process 
%%              Worker :: atom() - name of the worker
%%              JID :: string() - JID of this service
%%              Password :: string() - password of this service
%%              Port :: integer() - port from ejabberd service where this service must connect
%%              XMPPWaitUntilRestart :: integer() - time in milliseconds xmm_talker have to wait
%%                                              until it restart itself in case of connection interruption
%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%% Worker is a gen_server that must implement the following:
%%
%%              handle_cast({take_task, From, Body}, State) ->
%%                              if ant then gen_server:cast(?XMPP_TALKER, {task_finished, To, Data}),
%%                              and then terminate itself {stop, normal, State}
%%
%%              Where:
%%                      From :: string() - JID of the service that must receive message
%%                      Data :: string() - data that we must send back to the service
%%----------------------------------------------------------------------------------------
start_link(Name, Worker, JID, Password, Port, Wait) ->
	Result = gen_server:start_link({local, Name}, ?MODULE, [Worker, JID, Password, Port, Wait], []),
	spawn(fun() -> timer:sleep(1000), gen_server:cast(Name, {connect}) end),
	Result.


%%========================================================================================
%% gen_server callbacks
%%========================================================================================
%%----------------------------------------------------------------------------------------
%% Description: Initiates the server
%%              Worker :: atom() - name of the worker
%%              JID :: string() - JID of this service
%%              Password :: string() - password of this service
%%              Port :: integer() - port from ejabberd service where this service must connect
%%              XMPPWaitUntilRestart :: integer() - time in milliseconds xmm_talker have to wait
%%                                              until it restart itself in case of connection interruption
%%----------------------------------------------------------------------------------------
init([Worker, JID, Password, Port, Wait]) ->
	{ok, #state{
			worker = Worker,
			jid = JID,
			password = Password,
			port = Port,
			wait = Wait
	}}.

%%----------------------------------------------------------------------------------------
%% Description: Handling all non call/cast messages
%%----------------------------------------------------------------------------------------
% We got a message
handle_info(#received_packet{packet_type=message, raw_packet=Packet, type_attr=Type, from=From}, State) ->
	case Type of
		Type when Type =/= "error" ->
			Body = exmpp_message:get_body(Packet),
			%
			{PlayerID, ServerID, _} = From,
			Jid = binary_to_list(PlayerID) ++ "@" ++ binary_to_list(ServerID),
			% Here we creating worker process
			Worker = State#state.worker,
			{ok, Pid} = Worker:start(),
			gen_server:cast(Pid, {take_task, Jid, binary_to_list(Body)}),
			%
			{noreply, State};
		_ -> 
			% We got something strange (presense etc.), we don't care
			{noreply, State}
	end;
%
% Called when server is down/offline
handle_info({stream_error, _Reason}, State) ->
        % Server if down. Will try to restart soon...
        % Call restart_me. It will restart itself in State#state.wait milliseconds
        restart_me(State);
%
% We got something strange... We don't care
handle_info(_Info, State) ->
        {noreply, State}.

%%----------------------------------------------------------------------------------------
%% Description: Handling call messages
%%----------------------------------------------------------------------------------------
handle_call(_Request, _From, State) ->
        {reply, ok, State}.

%%----------------------------------------------------------------------------------------
%% Description: Handling cast messages
%%----------------------------------------------------------------------------------------
%% Message from worker
%%              worker_ok :: atom()
%%              From :: string() - whom to send the message
%%              Body :: string() - message to send
%%----------------------------------------------------------------------------------------
handle_cast({connect}, State) ->
	io:format("\nTrying to start the service...\n"),
	try
		% Start XMPP session: Needed to start service
		MySession = exmpp_session:start(),
		% Create XMPP ID (Session Key):
		[User, Server] = string:tokens(State#state.jid, "@"),
		MyJID = exmpp_jid:make(User, Server, random),
		% Create a new session with basic (digest) authentication:
        exmpp_session:auth(MySession, MyJID, State#state.password, password),
        % Try to connect in standard TCP
		_Result = exmpp_session:connect_TCP(MySession, Server, State#state.port),
		% login
		exmpp_session:login(MySession),
		% presence
		exmpp_session:send_packet(
				MySession, 
				exmpp_presence:set_status(exmpp_presence:available(), "Ready")),

		State1 = State#state{session = MySession},
		io:format("\nSeems that everything is ok\n"),
		{noreply, State1}
    catch
		% Wrong credentials. CRASH & HYSTERICS :-)
		throw:{auth_error, 'not-authorized'} ->
			erlang:error('Wrong credentials. Cannot work like this!!!');
		% Anything else
        _:_ -> 
			restart_me(State)
    end;
	
% Replying back
handle_cast({task_finished, To, Body}, State) ->
	Message = exmpp_message:chat(Body),
	Message1 = exmpp_xml:set_attribute(Message, <<"from">>, State#state.jid),
	Message2 = exmpp_xml:set_attribute(Message1, <<"to">>, To),
	exmpp_session:send_packet(State#state.session, Message2),
	{noreply, State};
% Anything else
handle_cast(_Msg, State) ->
	{noreply, State}.


%%----------------------------------------------------------------------------------------
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%----------------------------------------------------------------------------------------
terminate(_Reason, _State) ->
        ok.

%%----------------------------------------------------------------------------------------
%% Description: Convert process state when code is changed
%%----------------------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

%%----------------------------------------------------------------------------------------
%% Internal functions
%%              State :: #state - gen_server state
%%----------------------------------------------------------------------------------------
restart_me(State) ->
	io:format("\nJabber server is down. Will restart soon.\n"),
	Wait = State#state.wait,
	timer:sleep(Wait), % Sleep for Wait seconds
	{stop, error, State}.