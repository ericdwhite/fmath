-module(walk_server).

-behaviour(gen_server).

-export([start_link/0, lets_walk_for/1]).

-export([init/1, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, []}.

lets_walk_for(N) ->
    gen_server:call(?MODULE, {walk, N}).

%% callbacks
handle_call({walk, N}, _From, State) ->
    io:format("Walking for...~w~n", [N]),
    io:format("~w", [simplerandomwalk:walk(N)]),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

