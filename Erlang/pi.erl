-module(pi).

-export([montecarlo/1]).
-export([spawn_actor/2]).

-define(PROCESSES, 5).

%% Starts a montecarlo simulation with N iterations.
%% Uses a default number of actors to work out
%% the final C. Returns estimation of the value pi.
-spec montecarlo(integer()) -> integer().
montecarlo(N) ->
    io:format("Starting montecarlo simulation with N = ~p~n", [N]),
    Np = N div ?PROCESSES,
    Pids = create_actors(?PROCESSES, Np),
    C = wait_for_results(length(Pids)),
    Pi = pi_formula(C, N),
    io:format("Estimation of pi : ~p~n",[Pi]),
    Error = abs(math:pi() - Pi),
    io:format("Absolute Error: ~p~n", [Error]).

%% Spawns a workout of an asynchronous Cp.
-spec spawn_actor(pid(), integer()) -> {ok, pid()}.
spawn_actor(Manager, Np) ->
    Pid = spawn_link(pihelper, helper, [Manager, Np]),
    {ok, Pid}.

%% C: Coordinates within the target (circle)
%% N: Total number of generated coordinates.
%% C/N: Ratio of coordinates that falls within the target.
%%
%% C/N = Pi*Radio^2/4
%%
%% Given that Radio = 1:
%%
%% Pi = 4*C/N
pi_formula(C,N) ->
    4*C/N.

create_actors(Total, Np) ->
    create_actors(Total, Np, []).

create_actors(0, _, Pids) ->
    Pids;
create_actors(Total, Np, Pids) ->
    NewPid = new_actor(Np),
    create_actors(Total-1, Np, [NewPid | Pids]).
    
new_actor(Np) ->
    {ok, Pid} = spawn_actor(self(), Np),
    Pid.

wait_for_results(N) ->
    wait_for_results(N, 0).

wait_for_results(0, C) ->
    C;
wait_for_results(N, C) ->
    receive
        {c, NewC, From} ->
           %% io:format("Cp = ~p from ~p", [NewC, From]),
            wait_for_results(N-1, C+NewC)
    after
        1000 ->
            timeout
    end.
