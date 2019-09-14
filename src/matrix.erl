-module(matrix).
-export([login/1, get_message_batch/2, sync_since/3, create_room/2, put_text_message/5]).
%% -compile(export_all).

%% everything in the matrix API is json
%% strings must be UTF-8 so make sure your
%% keys and values are atoms or binaries
post(Url, Map) ->
    Body = jsone:encode(Map),
    httpc:request(post,
                  {Url, [], "application/json", Body},
                  [],
                  []).

%% returns a http response with a json body that
%% includes the access token and other user details
login(Creds) ->
    {Homeserver, User, Pwd} = Creds,
    URL = Homeserver ++ "/_matrix/client/r0/login",
    ReqBody = #{
                type => <<"m.login.password">>,
                identifier => #{
                                type => <<"m.id.user">>,
                                user => binary:list_to_bin(User)
                               },
                password => binary:list_to_bin(Pwd)
               },

    case post(URL, ReqBody) of
        {ok, {{_Version, 200, _ReasonPhrase}, _Headers, RespBody}} ->
            {good, jsone:decode(list_to_binary(RespBody))};
        {ok, BadResponse} ->
            {bad, BadResponse};
        ErrorWithReason -> ErrorWithReason
    end.

%% Token should be a string, as it goes in a query param
%% returns map with room_id
create_room(Homeserver, Token) ->
    Url = Homeserver ++ "/_matrix/client/r0/createRoom?access_token=" ++ Token,
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, RespBody}} =
        post(Url, #{}),
    jsone:decode(list_to_binary(RespBody)).


get_with_auth(Url,Token) ->
    httpc:request(get, {Url, [{"Authorization", "Bearer " ++ Token}]},[],[]).

put_with_auth(Url,Token,BodyMap) ->
    Body = jsone:encode(BodyMap),
    httpc:request(put,
                  {Url,[{"Authorization", "Bearer " ++ Token}], "application/json", Body},
                  [],
                  []).

put_text_message(Server,Token,Room,Message,TxnId) ->
    Url = Server
        ++ "/_matrix/client/r0/rooms/"
        ++ Room
        ++ "/send/m.room.message/"
        ++ lists:flatten(io_lib:format("~p", [TxnId])),

    Body = #{ <<"msgtype">> => <<"m.text">>,
              <<"body">> => Message
            },

    put_with_auth(Url,Token,Body).


get_message_batch(Server,Token) ->
    Url = Server ++ "/_matrix/client/r0/sync",
    case get_with_auth(Url, Token) of
        {ok, {{_, 200, _}, _, Body}} ->
            Decoded = jsone:decode(list_to_binary(Body)),
            maps:get(<<"next_batch">>,Decoded, none);
        _AnythingElse -> none
    end.

sync_since(Server,Token,Since) ->
    Url = unicode:characters_to_list(
            [
             Server,
             "/_matrix/client/r0/sync?&since=",
             Since,
             "&full_state=false"
            ]
           ),
    case get_with_auth(Url, Token) of
        {ok, {{_, 200, _}, _, Body}} ->
            %% io:format(Body),
            Decoded = jsone:decode(list_to_binary(Body)),
            {good,
             maps:get(<<"next_batch">>, Decoded),
             pluck_messages(Decoded)};
        _ -> {bad, oops}
    end.

pluck_messages(JSON) ->
    Rooms = maps:get(<<"rooms">>, JSON),
    Join = maps:get(<<"join">>, Rooms, #{}),
    %% TODO: Join is a map with keys that are room ids. probably iterate over all
    %% this hardcodes the matrix bot dev room
    MBotDev = maps:get(<<"!TBaDphVIRUZQyIfJfB:matrix.hrlo.world">>, Join, #{}),
    Timeline = maps:get(<<"timeline">>, MBotDev, #{}),
    maps:get(<<"events">>, Timeline, []).

