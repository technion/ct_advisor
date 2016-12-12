-module(scheduler).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
-define(INTERVAL, 2000).

init(_Args) ->
    erlang:send_after(?INTERVAL, self(), scheduler),
    % This hack just gives us a valid PID to set as the first State
    Pid = spawn(fun() -> lager:debug("Initializing scheduler") end),
    {ok, Pid}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(scheduler, State) ->
    Pid = case process_info(State) of
    undefined ->
        spawn(leaf_iterate, scheduled_check, []);
    _ ->
        lager:debug("Process ~p already running, sleeping", [State]),
        State
    end,
    erlang:send_after(?INTERVAL, self(), scheduler),
    {noreply, Pid};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

