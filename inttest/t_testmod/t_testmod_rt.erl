-module(t_testmod_rt).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

files() ->
    [{copy, "rebar.config", "rebar.config"},
     {copy, "../../src/remote_plugin_loader.erl", 
            "build/plugins/remote_plugin_loader.erl"},
     {copy, "global.config", ".home_folder/.rebar/config"},
     {create, "ebin/testmod.app", app(testmod, [testmod])}].

run(Dir) ->
    retest_log:log(debug, "Running in Dir: ~s~n", [Dir]),
    ?assertMatch({ok, _}, retest:sh("rebar check-deps -v", [])),
    ?assertMatch({ok, _}, retest:sh("rebar install-plugins -v", 
                                    [{env, [{"HOME", ".home_folder"}]}])),
    ?assertMatch(true, filelib:is_regular("build/plugins/rebar_skip_deps.erl")),
    ok.

%%
%% Generate the contents of a simple .app file
%%
app(Name, Modules) ->
    App = {application, Name,
           [{description, atom_to_list(Name)},
            {vsn, "1"},
            {modules, Modules},
            {registered, []},
            {applications, [kernel, stdlib]}]},
    io_lib:format("~p.\n", [App]).

