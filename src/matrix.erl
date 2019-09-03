-module(matrix).
-export([login_user/3]).

%% returns a http response with a json body that
%% includes the access token and other user details
login_user(Homeserver, User, Pwd) ->
    URL = Homeserver ++ "/_matrix/client/r0/login",
    Type = "application/json",
    Body = jsone:encode(
             #{
               type => <<"m.login.password">>,
               identifier => #{
                               type => <<"m.id.user">>,
                               user => User
                              },
               password => Pwd
              }
             ),
    inets:start(),
    ssl:start(),

    httpc:request(post, {URL, [], Type, Body}, [], []).
