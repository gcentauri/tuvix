-module(tuvix).
-export([start/3,start/4,echo/3]).

%% Server - homeserver matrix.yada.yada
start(Server,User,Password) ->
    start(Server,User,Password, #{poll_min => 1000, poll_max => 60000}).

start(Server,User,Password,Config) ->
    inets:start(),
    ssl:start(),
    case matrix:login({Server,User,Password}) of
        {good, ResponseBody} ->
            Token = binary:bin_to_list(maps:get(<<"access_token">>, ResponseBody)),
            NewConfig = maps:merge(Config, #{server => Server, token => Token, user => User}),
            {started, spawn(fun() -> start_loop(NewConfig) end)};
        Any ->  Any
    end.

start_loop(Config) ->
    start_loop(maps:get(user,Config), maps:get(token,Config), maps:get(server,Config), Config).

start_loop(User,Token,Server,Config) ->
    PollMin = maps:get(poll_min, Config, 500),
    PollConfig = maps:merge(Config, #{bot => self(),
                                      server => Server,
                                      token => Token,
                                      wait => PollMin}),
    {ok, _} = initialize_polling_agent(PollConfig),
    BotState = #{ user => list_to_binary( "@" ++ User ++ ":" ++ Server),
                  token => Token,
                  server => Server
                },
    Actions = [{"echo", fun(_X) -> true end, fun(A,B,C) -> echo(A,B,C) end}],
    loop(BotState,Actions).

initialize_polling_agent(Config) ->
    case matrix:get_message_batch(maps:get(server, Config), maps:get(token, Config)) of
        none ->
            oh_no;
        BatchToken ->
            {ok, spawn(
                   fun() ->
                           polling_agent(maps:put(since,
                                                  binary:bin_to_list(BatchToken),
                                                  Config))
                   end)}
    end.

new_messages(Bot, Messages) ->
    Formatted = lists:filter(
                  fun(Msg) ->
                          case maps:get(<<"content">>, Msg, none) of
                              none -> false;
                              Content ->
                                  maps:get(<<"msgtype">>, Content) =:= <<"m.text">>
                          end
                  end, Messages),
    Bot ! {new_messages, Formatted}.

polling_agent(Config) ->
    Wait = maps:get(wait,Config),

    timer:sleep(Wait),

    Token = maps:get(token,Config),
    Server = maps:get(server,Config),
    Since = maps:get(since,Config),

    case matrix:sync_since(Server, Token, Since) of
        {good, NewSince, []} ->
            PollMax = maps:get(poll_max, Config),
            NewConfig = maps:merge(Config, #{ wait => lengthen_wait(Wait, PollMax),
                                              since => NewSince }),
            polling_agent(NewConfig);
        {good, NewSince, Messages} ->
            new_messages(maps:get(bot,Config), Messages),
            PollMin = maps:get(poll_min, Config),
            NewConfig = maps:merge(Config, #{ wait => PollMin,
                                              since => NewSince}),
            polling_agent(NewConfig);
        _SomethingBad ->
            io:format("Something Bad Happened while Polling....~n"),
            polling_agent(Config)
    end.

lengthen_wait(Wait,MaxWait) ->
    min(MaxWait, trunc(Wait + (MaxWait - Wait) / 2)).

request_txn_id(Bot) ->
    Bot ! {get_txn_id, self()},
    receive
        {txn_id, TxnId} ->
            TxnId
    end.

loop(State,Actions) ->
    receive
        {get_txn_id, Caller} ->
            TxnId =  maps:get(txn_id, State, 1),
            Caller ! {txn_id, TxnId},
            loop(maps:put(txn_id, 1 + TxnId, State), Actions);
        {update_state , Key , Val} ->
            loop(maps:put(Key,Val,State), Actions);
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

handle_messages(_State,_Actions,[]) -> no_op;
handle_messages(State,Actions,[M|Msgs]) ->
    Bot = self(),
    lists:foreach(fun({_,Pred,Action}) ->
                          case Pred(M) of
                              true ->
                                  spawn(fun() -> Action(Bot,State,M) end);
                              _ -> no_op
                          end
                  end, Actions),
    handle_messages(State,Actions,Msgs).

remove_action_by_name(Name,Actions) ->
    FilterByName = fun(X) ->
                           case X of
                               {Name,_,_} -> false;
                               _ -> true
                           end
                   end,
    lists:filter(FilterByName, Actions).

%% {\"events\": [{\"type\": \"m.room.message\", \"sender\": \"@shoshin:matrix.hrlo.world\", \"content\": {\"msgtype\": \"m.text\", \"body\": \"HEY TUVIX!!! ITS CHAKOTAY!!! CAN YOU HEAR ME!!!????\"}, \"origin_server_ts\": 1568258310194, \"unsigned\": {\"age\": 18603}, \"event_id\": \"$DdHy9qbVN6Fqw-uQzrs8nbef-oF_2iDeW8bR0wYteJw\"}]

parse_message(Msg) ->
    Sender = maps:get(<<"sender">>, Msg),
    Content = maps:get(<<"content">>, Msg),
    Text = maps:get(<<"body">>, Content),
    {Sender,Text}.

echo(Bot,State,Msg) ->
    User = maps:get(user, State),
    Server = maps:get(server, State),
    Token = maps:get(token, State),
    Room = "!TBaDphVIRUZQyIfJfB:matrix.hrlo.world",
    TxnId = request_txn_id(Bot),
    {Sender, Text} = parse_message(Msg),
    Notice = is_mention(Text),
    if
        Notice ->
            case Sender of
                User -> no_op;
                _ -> matrix:put_text_message(Server,Token,Room,Text,TxnId)
            end;
        true ->
            nothing
    end.

is_mention(Msg) ->
    Words = re:split(Msg, " "),
    lists:any(fun(S) -> <<"@tuvix">> =:= S end, Words).
