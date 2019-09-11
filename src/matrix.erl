-module(matrix).
-export([login/1, get_message_batch/2, sync_since/3, create_room/2]).
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




get_message_batch(Server,Token) ->
    Url = Server ++ "/_matrix/client/r0/sync?access_token=" ++ Token,
    case httpc:request(Url) of
        {ok, {{_, 200, _}, _, Body}} ->
            Decoded = jsone:decode(Body),
            maps:get(<<"next_batch">>,Decoded, none);
        _AnythingElse -> none
    end.


sync_since(Server,Token,Since) ->
    Url = string:join([Server, 
                       "/_matrix/client/r0/sync?access_token=",
                      Token,
                      "&since=", 
                       Since,
                      "&full_state=false"]),
    case httpc:request(Url) of
        {ok, {{_, 200, _}, _, Body}} ->
            Decoded = jsone:decode(Body),
            {good, 
             maps:get(<<"next_batch">>, Decoded),
             pluck_messages(Decoded)};
        _ -> {bad, oops}
    end.

pluck_messages(JSON) ->
    Rooms = maps:get(<<"rooms">>, JSON),
    Join = maps:get(<<"join">>, Rooms),
    Timeline = maps:get(<<"timeline">>, Join),
    maps:get(<<"events">>, Timeline).

