-module(scrapper_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
  scrapper_ssup:start_link().

stop(_State) ->
  ok.
