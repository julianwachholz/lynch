-module(lynch_app).
-behaviour(application).

-export([start/2, stop/1]).


start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", cowboy_static, {priv_file, lynch, "index.html"}},
            {"/static/[...]", cowboy_static, {priv_dir, lynch, "static"}},
            {"/websocket", ws_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}],
        [{env, [{dispatch, Dispatch}]}]),
    lynch_sup:start_link().


stop(_State) -> ok.
