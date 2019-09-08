-module(tuvix).
-export([start/3,start/2]).

start(Server,User,Password) ->
    start(Server,User,Password, #{poll_min => 1000, poll_max => 60000}).

start(Server,User,Password,Config) ->
    inets:start(),
    ssl:start(),
    case matrix:login({Server,User,Password}) of
        {good, ResponseBody} ->
            spawn(fun() -> start_loop(Token,Server,Config) end);
        Any ->
            Any
    end.

start_loop(Token,Server,Config) ->
    PollMin = maps:get(poll_min, Config, 500),
    PollConfig = maps:merge(Config, #{bot => self(),
                                      server => Server,
                                      token => Token,
                                      wait => PollMin}),
    spawn(fun() -> initialize_polling_agent(PollConfig) end),
    loop(null_state,[]).

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
        {Pid, Response} ->
            Response
    end.

initialize_polling_agent(Config) ->
    pollling_agent(Config).

pollling_agent(Config) ->
    Wait = maps:get(wait,Config),

    timer:sleep(Wait),

    Token = maps:get(token,Config),
    Server = maps:get(server,Config),
    Since = maps:get(since,Config),

    case matrix:sync_since(Server, Token, Since) of
        {good, NewSince, []} ->
            PollMax = maps:get(poll_max, Config),
            NewConfig = maps:merge(Config, #{ wait => lengthen_wait(Wait, PollMax),
                                              since => NewSince
                                            }),
            pollling_agent(NewConfig);
        {good, NewSince, Messages} ->
            new_messages(maps:get(bot,Config), Messages),
            PollMin = maps:get(poll_min, Config),
            NewConfig = maps:merge(Config, #{ wait =>shorten_wait(Wait,PollMin),
                                              since => NewSince}),
            pollling_agent(NewConfig);
        SomethingBad ->
            io:format("Something Bad Happened while Polling....~n"),
            pollling_agent(Config)
    end.



loop(State,Actions) ->
    receive
        {update_state, NewState} ->
            loop(NewState,Actions);
        {get_state, Caller} ->
            Caller ! State,
            loop(State,Actions);
        {add_action, Name, Pred, Action} ->
            loop(State,[{Name,Pred,Action}|Actions]);
        {drop_action, Name} ->
            loop(State,remove_action_by_name(Name,Actions));
        {new_messages, Msgs} ->
            handle_messages(State,Actions,Msgs),
            loop(State,Actions);
        {From, Other} ->
            From ! {error, request_unknown, Other},
            loop(State,Actions)
    end.


remove_action_by_name(Name,Actions) ->
    FilterByName = fun(X) ->
                           case X of
                               {Name,_,_} -> false;
                               _ -> true
                           end
                   end,
    lists:filter(FilterByName, Actions).
