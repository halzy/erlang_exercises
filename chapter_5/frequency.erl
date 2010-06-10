-module(frequency).
-export([start/0, stop/0, allocate/0, deallocate/1, deallocate_all/0]).

-export([init/0]).

-define(SERVER, ?MODULE).

-type frequency() :: pos_integer().
-type used_frequency() :: { frequency(), pid() }.

-record(state, {
	  free = [] :: [frequency()],
	  allocated = [] :: [used_frequency()], 
	  shutting_down = false :: boolean(),
	  max_allocations = 3 :: pos_integer()
	 }).

%% These are the start functions used to create and
%% initialize the server.

start() ->
    register(?SERVER, spawn(?MODULE, init, [])).

-spec init() -> ok.
init() ->
    State = #state{
      free=get_frequencies()
     },
    loop(State).

% Hard Coded
-spec get_frequencies() -> [frequency()].
get_frequencies() ->
    [10, 11, 12, 13, 14, 15].

%% The client Functions

-spec stop() -> ok.
stop() ->
    call(stop).

-spec allocate() -> {error, atom()} | pos_integer().
allocate() ->
    call(allocate).

-spec deallocate(frequency()) -> ok.
deallocate(Freq) ->
    call({deallocate, Freq}).

-spec deallocate_all() -> ok.
deallocate_all() ->
    call(deallocate_all).


%% We hide all message passing and the message
%% protocol in a functional interface

call(Message) ->
    ?SERVER ! {request, self(), Message},
    receive
	{reply, Reply} ->
	    Reply
    end.

%% The Main Loop

-spec loop(#state{}) -> ok.
%% [bgh] if we are shutting down, and have no outstanding frequencies, end
loop(#state{shutting_down=true, allocated=[]}) ->
    ok;
loop(#state{shutting_down=ShuttingDown}=State) ->
    receive
	{request, Pid, allocate} when false==ShuttingDown ->
	    {UpdatedState, Reply} = allocate(State, Pid),
	    reply(Pid, Reply),
	    loop(UpdatedState);
	{request, Pid, stop} when false==ShuttingDown ->
	    reply(Pid, ok),
	    loop(State#state{shutting_down=true});
	{request, Pid, {deallocate, Freq}} ->
	    UpdatedState = deallocate(State, Pid, Freq),
	    reply(Pid, ok),
	    loop(UpdatedState);
	{request, Pid, deallocate_all} ->
	    UpdatedState = deallocate_all(State, Pid),
	    reply(Pid, ok),
	    loop(UpdatedState);
	{request, Pid, _} when true == ShuttingDown ->
	    reply(Pid, {error, shutting_down}),
	    loop(State)
    end.

reply(Pid, Reply) ->
    Pid ! {reply, Reply}.

%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

-spec allocate(#state{free::[]}, pid()) ->
		      {#state{}, {error, no_frequency}};
	      (#state{free::[frequency(),...]}, pid()) -> 
		      {#state{}, {error, max_allocations}} | 
		      {#state{}, {ok, frequency()}}.
allocate(#state{free=[]}=State, _Pid) ->
    {State, {error, no_frequency}};
allocate(#state{free=[Freq|Free], allocated=Allocated}=State, Pid) ->
    case can_allocate(Allocated, Pid, State#state.max_allocations) of
	false ->
	    {State, {error, max_allocations}};
	true ->
	    {State#state{free=Free, allocated=[{Freq, Pid}|Allocated]}, {ok, Freq}}
    end.

-spec deallocate(#state{}, pid(), frequency()) -> #state{}.
deallocate(#state{free=Free, allocated=Allocated}=State, Pid, Freq) ->
    case lists:keytake(Freq, 1, Allocated) of
	false ->
	    State;
	{value, {Freq, Pid}, NewAllocated} ->
	    State#state{free=[Freq|Free], allocated=NewAllocated};
	{value, {Freq, _WrongPid}, _NewAllocated} ->
	    State
    end.

-spec pid_match(used_frequency(), pid()) -> boolean().
pid_match({_Freq, Pid}, Pid) -> true;
pid_match({_Freq, _OtherPid}, _Pid) -> false.

-spec deallocate_all(#state{}, pid()) -> #state{}.
deallocate_all(#state{free=Free, allocated=Allocated}=State, Pid) ->
    {ToDeallocate, UpdatedAllocated} = lists:partition(fun(Elem) -> pid_match(Elem, Pid) end, Allocated),
    {ReleasedFreqs, _Pids} = lists:unzip(ToDeallocate),
    UpdatedFree = ReleasedFreqs ++ Free,
    State#state{free=UpdatedFree, allocated=UpdatedAllocated}.

-spec can_allocate([used_frequency()], pid(), pos_integer()) -> boolean().
can_allocate(Allocated, Pid, MaxFrequencies) ->
    {PidFrequencies, _Other} = lists:partition(fun(Elem) -> pid_match(Elem, Pid) end, Allocated),
    length(PidFrequencies) < MaxFrequencies.


