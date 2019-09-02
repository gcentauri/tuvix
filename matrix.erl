-module(matrix).
-export([login_grant/0]).

login_grant() ->
    URL = "https://matrix.hrlo.world/_matrix/client/r0/login",
    Type = "application/json",
    %% replace username and password
    Body = "{\"type\":\"m.login.password\", \"identifier\": {\"type\":\"m.id.user\", \"user\":\"username\"}, \"password\":\"password\"}",

    inets:start(),
    ssl:start(),

    httpc:request(post, {URL, [], Type, Body}, [], []).
