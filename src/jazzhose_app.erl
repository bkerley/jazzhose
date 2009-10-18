%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the jazzhose application.

-module(jazzhose_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for jazzhose.
start(_Type, _StartArgs) ->
    jazzhose_deps:ensure(),
    jazzhose_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for jazzhose.
stop(_State) ->
    ok.
