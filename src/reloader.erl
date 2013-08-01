%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created :  
-module(reloader).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start/0, stop/0]).
-export([set_debug/0, set_info/0]).
-export([reload/0]).


%% doc starrt the application
%%
	start() ->
		application:start(lager),		
	  	application:start(?MODULE).

%% doc stop the application
	stop() ->
		application:stop(lager),		
		application:stop(?MODULE).

	reload() ->
		code_reloader:reload().

	set_debug() ->
		lager:set_loglevel(lager_console_backend, debug).

	set_info() ->
		lager:set_loglevel(lager_console_backend, info).	
%% --------------------------------------------------------------------
%% record definitions
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
