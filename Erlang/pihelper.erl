-module(pihelper).

-export([helper/2]).
-export([the_square/0]).
-export([the_square/1]).

helper(Manager, Np) ->
    C = the_square(Np),
    Manager ! {c, C, self()},
    ok.

-spec the_square(integer()) -> integer().
the_square(Np) ->
    the_square(Np,0).

the_square(0, Cp) ->
    Cp;
the_square(Nr, Cp) ->
    case the_square() of
        true ->
            the_square(Nr-1, Cp+1);
        false ->
            the_square(Nr-1, Cp)
    end.

-spec the_square() -> boolean().
the_square() ->
    {X,Y} = {random:uniform(), random:uniform()},
    X*X + Y*Y =< 1.
