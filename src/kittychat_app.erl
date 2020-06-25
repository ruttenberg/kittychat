%%%-------------------------------------------------------------------
%% @doc kittychat public API
%% @end
%%%-------------------------------------------------------------------

-module(kittychat_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  Dispatch = cowboy_router:compile([
                                     {'_', [
                                       {"/", kitty_handler, []},
                                       {"/:path", kitty_handler, []}
                                     ]}
                                   ]),
  {ok, _} = cowboy:start_clear(kitty_http_listener,
                               [{port, 8080}],
                               #{env => #{dispatch => Dispatch}}
  ),
    kittychat_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
