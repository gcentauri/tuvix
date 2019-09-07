-module(tuvix).
-export([start/0, login/2, loop/0]).

start() ->
    inets:start(),
    ssl:start(),
    spawn(tuvix, loop, []).

login(Pid, Creds) ->
    rpc(Pid, {login, Creds}).

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
        {Pid, Response} ->
            Response
    end.

loop() ->
    receive
        {From, {login, Creds}} ->
            From ! {self(), matrix:login(Creds)},
            loop();
        {From, Other} ->
            From ! {error, Other},
            loop()
    end.
