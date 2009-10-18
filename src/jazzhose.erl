%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(jazzhose).
-author('author <author@example.com>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
        
%% @spec start() -> ok
%% @doc Start the jazzhose server.
start() ->
    jazzhose_deps:ensure(),
    ensure_started(crypto),
    application:start(jazzhose).

%% @spec stop() -> ok
%% @doc Stop the jazzhose server.
stop() ->
    Res = application:stop(jazzhose),
    application:stop(crypto),
    Res.
