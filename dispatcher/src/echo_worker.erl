%%%-------------------------------------------------------------------
%%% File    : echo_worker.erl
%%% Author  : Max Burinov <bourinov@gmail.com>
%%% Description : Echo worker
%%%
%%% Created :  18.10.2011
%%%-------------------------------------------------------------------
-module(echo_worker).

-behaviour(gen_server).

-include("config.hrl").

-export([start/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% There is no need for state         
%-record(state, {}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Description: Starts the server
%%--------------------------------------------------------------------
start() ->
    gen_server:start(?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Description: Initiates the server
%%--------------------------------------------------------------------
init(_Args) ->
	{ok, dict:new()}.

%%--------------------------------------------------------------------
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
	{reply, ok, State}.

%%--------------------------------------------------------------------
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({take_task, From, Body}, State) ->
	%
	% Here you do all your job with body...
	%
	gen_server:cast(?XMPP_TALKER, {task_finished, From, Body}),
	{stop, normal, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
	ok.

%%--------------------------------------------------------------------
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
