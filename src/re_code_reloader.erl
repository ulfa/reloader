%% Copyright 2010 Ulf Angermann
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created : 
%%% -------------------------------------------------------------------
-module(re_code_reloader).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% External exports

%% gen_server callbacks

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).
-export([start/0]).
-export([reload/0]).
%% ====================================================================
%% External functions
%% ====================================================================
reload() ->
    gen_server:call(?MODULE, reload).
%% --------------------------------------------------------------------
%% record definitions
%% --------------------------------------------------------------------
-record(state, {}).
%% ====================================================================
%% Server functions
%% ====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start() ->
    start_link().
%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(reload, From, State) ->
    Changed_Modules = get_changed_modules(),
    reload_modules(Changed_Modules),
    {reply, Changed_Modules, State};

handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(reload, State) ->
    Changed_Modules = get_changed_modules(),
    reload_modules(Changed_Modules),
    {noreply, State};

handle_info(Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
get_changed_modules() ->
    [Module || {Module, File} <- code:all_loaded(), is_not_system_module(Module), is_beamfile(File), is_old(Module,File)].

reload_modules([]) ->
    [];
reload_modules(Changed_Modules) ->
    [load_module(Module) || Module <- Changed_Modules].

is_old(Module,File) ->
    loaded_time(Module) < not_yet_loaded_time(File).

is_beamfile(File) ->
    ok =:= element(1,file:read_file_info(File)) andalso ".beam" =:= filename:extension(File).

not_yet_loaded_time(File) ->
    {ok,{_,[{_,I}]}} = beam_lib:chunks(File,[compile_info]),
    proplists:get_value(time,I).

loaded_time(Module) ->
    proplists:get_value(time, Module:module_info(compile)).

load_module(Module) ->
    lager:info("reload ~p ", [Module]),
    code:purge(Module),
    code:load_file(Module).

is_not_system_module(Module) ->
    [] =:= [X || X <- get_system_modules(), X =:= Module].
get_system_modules() ->
        [
        appmon,
        asn1,
        common_test,
        compiler,
        crypto,
        debugger,
        dialyzer,
        docbuilder,
        edoc,
        erl_interface,
        erts,
        et,
        eunit,
        gs,
        hipe,
        inets,
        inets,
        inviso,
        jinterface,
        kernel,
        mnesia,
        observer,
        orber,
        os_mon,
        parsetools,
        percept,
        pman,
        reltool,
        runtime_tools,
        sasl,
        snmp,
        ssl,
        stdlib,
        syntax_tools,
        test_server,
        toolbar,
        tools,
        tv,
        webtool,
        wx,
        xmerl,
        zlib
    ].
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.