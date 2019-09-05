-module(matrix).
-export([login_user/3]).
-compile(export_all).

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
login_user(Homeserver, User, Pwd) ->
    inets:start(),
    ssl:start(),

    URL = Homeserver ++ "/_matrix/client/r0/login",
    ReqBody = #{
                type => <<"m.login.password">>,
                identifier => #{
                                type => <<"m.id.user">>,
                                user => User
                               },
                password => Pwd
               },

    {ok, {{Version, 200, ReasonPhrase}, Headers, RespBody}} =
        post(URL, ReqBody),

    jsone:decode(list_to_binary(RespBody)).

%% Token should be a string, as it goes in a query param
%% returns map with room_id
create_room(Homeserver, Token) ->
    Url = Homeserver ++ "/_matrix/client/r0/createRoom?access_token=" ++ Token,
    {ok, {{Version, 200, ReasonPhrase}, Headers, RespBody}} =
        post(Url, #{}),
    jsone:decode(list_to_binary(RespBody)).

