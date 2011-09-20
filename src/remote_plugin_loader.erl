%% -----------------------------------------------------------------------------
%%
%% Copyright (c) 2011 Tim Watson (watson.timothy@gmail.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -----------------------------------------------------------------------------
-module(remote_plugin_loader).

-export(['plugins:clean'/2, 'plugins:install'/2]).

-define(CONFIG(C, K, D), rebar_config:get_local(C, K, D)).
-define(DEFAULT_PLUGIN_DIR, filename:join(rebar_utils:get_cwd(), "plugins")).
-define(DEBUG(Msg, Args), ?LOG(debug, Msg, Args)).
-define(WARN(Msg, Args), ?LOG(warn, Msg, Args)).
-define(LOG(Lvl, Msg, Args), rebar_log:log(Lvl, Msg, Args)).

%%
%% Plugin API
%%

'plugins:clean'(Config, _AppFile) ->
    PluginDir = ?CONFIG(Config, plugin_dir, ?DEFAULT_PLUGIN_DIR),
    Cache = filename:join([rebar_utils:get_cwd(), PluginDir, "plugins.cache"]),
    case file:read_file(Cache) of
        {ok, Bin} ->
            [ rebar_file_utils:rm_rf(F) || 
                    F <- string:tokens(binary_to_list(Bin), "\n") ],
            ?DEBUG("Clearing plugin cache ...~n", []),
            file:delete(Cache);
        Other ->
            ?WARN("Cannot find plugin cache file: ~p~n", [Other])
    end,
    ok.

'plugins:install'(Config, _AppFile) ->
    case get({remote_plugin_loader, status}) of
        complete -> ok;
        _ ->
            case is_pending_clean() of
                true ->
                    ok;
                false ->
                    Plugins = lists:flatten(rebar_config:get_all(Config, plugins)),
                    PluginDir = ?CONFIG(Config, plugin_dir, ?DEFAULT_PLUGIN_DIR),
                    case missing_plugins(Plugins, PluginDir) of
                        [] -> ok;
                        MissingPlugins ->
                            ?DEBUG("Checking for missing plugins ~p~n", 
                                    [MissingPlugins]),
                            Remotes = ?CONFIG(Config, plugin_remotes, []),
                            ?DEBUG("Remotes set to ~p~n", [Remotes]),
                            [ process(Missing, get_remote(Missing, Remotes), 
                                PluginDir, Config) || Missing <- MissingPlugins ],
                            put({remote_plugin_loader, status}, complete),
                            ok
                    end
            end
    end.

%%
%% Internal API
%%

is_pending_clean() ->
    lists:member('plugins:clean', rebar_config:get_global(issued_commands, [])).

get_remote(Missing, Remotes) ->
    Found = proplists:get_value(Missing, Remotes),
    ?DEBUG("Remote for ~p: ~p~n", [Missing, Found]),
    Found.

missing_plugins(Plugins, PluginDir) ->
    Erls = string:join([atom_to_list(M)++"\\.erl" || M <- Plugins], "|"),
    RE = "^" ++ Erls ++ "\$",
    BaseDir = rebar_config:get_global(base_dir, []),
    Sources = rebar_utils:find_files(PluginDir, RE, false)
        ++ rebar_utils:find_files(BaseDir, RE, false),
    ModFiles = [ filename:basename(Src, ".erl") || Src <- Sources ],
    ModuleNames = [ list_to_atom(M) || M <- ModFiles ],
    Plugins -- ModuleNames.

mod_to_erl(Mod) ->
    atom_to_list(Mod) ++ ".erl".

process(Missing, [H|_]=Url, PluginDir, Config) when is_integer(H) ->
    case lists:prefix("http", Url) of
        true -> fetch(Url, filename:join(PluginDir, mod_to_erl(Missing)), Config);
        false -> process(Missing, list_to_tuple(string:tokens(Url, "/")), 
                         PluginDir, Config)
    end;
process(Missing, {User, Tree}, PluginDir, Config) ->
    process(Missing, {User, Tree, atom_to_list(Missing)}, PluginDir, Config);
process(Missing, {User, Tree, Repo}, PluginDir, Config) ->
    SourceName = mod_to_erl(Missing),
    Url = string:join(["https://raw.github.com", User, Repo,
                      Tree, "src", SourceName], "/"),
    fetch(Url, filename:join(PluginDir, SourceName), Config);
process(Missing, Other, _PluginDir, _Config) ->
    ?WARN("Invalid config for ~p: [~p]~n", [Missing, Other]).

fetch(Url, Target, Config) ->
    case get({?MODULE, httpc}) of
        started ->
            ok;
        _ ->
            inets:start(),
            application:load(sasl),
            application:set_env(sasl, sasl_error_logger, false),
            lists:map(fun application:start/1, [sasl, crypto, public_key, ssl]),
            Timeout = ?CONFIG(Config, remote_net_timeout, 6000),
            case ?CONFIG(Config, remote_proxy_host, undefined) of
                undefined ->
                    httpc:set_options([{timeout, Timeout},
                                       {connect_timeout, Timeout}]);
                Host ->
                    Port = ?CONFIG(Config, remote_proxy_port, "8080"),
                    httpc:set_options([{proxy, {{Host, Port}, ["localhost"]}},
                                       {timeout, Timeout},
                                       {connect_timeout, Timeout}])
            end,
            put({?MODULE, httpc}, started)
    end,
    rebar_utils:ensure_dir(Target),
    Request = {Url, [{"User-Agent", "Rebar-Remote-Plugin-Loader"}]},
    ?DEBUG("Attempting to fetch ~s into ~s~n", [Url, Target]),
    case httpc:request(get, Request, [{relaxed, true}], 
                                     [{stream, Target}, {full_result, true}]) of
        {ok, saved_to_file} ->
            CacheFile = filename:join(filename:dirname(Target), "plugins.cache"),
            ok = file:write_file(CacheFile, Target ++ "\n", [append]),
            ?DEBUG("Successfully loaded remote plugin!~n", []);
        Error ->
            ?WARN("Error trying to load remote plugin: ~p~n", [Error])
    end.


