%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2012 Christopher Meiklejohn (cmeiklejohn@basho.com)
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
%% -------------------------------------------------------------------

%% The rebar_uglifyjs_compressor module is a plugin for rebar that compresses
%% javascript files using UglifyJS.
%%
%% Configuration options should be placed in rebar.config under
%% 'uglifyjs'.  Available options include:
%%
%%  doc_root: where to find javascript files to compile
%%            "priv/assets/javascripts" by default
%%
%%  out_dir: where to put compressed javascript files
%%           "priv/www/javascripts" by default
%%
%%  source_ext: the file extension the javascript sources have
%%              ".js" by default
%%
%%  module_ext: characters to append to the javascript's file name
%%              ".min" by default
%%
%% The default settings are the equivalent of:
%%   {uglifyjs, [
%%               {doc_root,   "priv/assets/javascripts"},
%%               {out_dir,    "priv/www/javascripts"},
%%               {source_ext, ".js"},
%%               {module_ext, ".min"}
%%              ]}.

-module(rebar_uglifyjs_compressor).

-export([compile/2]).

-include("rebar.hrl").

%% ===================================================================
%% Public API
%% ===================================================================
%
compile(Config, _AppFile) ->
    Options = options(Config),
    FileGlob = lists:flatten([".*\\", option(source_ext, Options), "\$"]),
    case rebar_utils:find_files(option(doc_root, Options), FileGlob) of
        [] ->
            ok;
        FoundFiles ->
            case uglifyjs_is_present() of
                true ->
                    Targets = [{Source, target_file(Source, Options)} || Source <- FoundFiles],
                    compress_each(Targets);
                false ->
                    ?ERROR(
                        "~n===============================================~n"
                        " You need to install uglify-js to compress assets~n"
                        " Please run the following: npm install uglify-js~n"
                        "===============================================~n~n",
                        []),
                    ?ABORT
            end
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================

options(Config) ->
    rebar_config:get(Config, uglifyjs, []).

option(Option, Options) ->
    proplists:get_value(Option, Options, default(Option)).

default(doc_root) -> "priv/assets/javascripts";
default(out_dir)  -> "priv/www/javascripts";
default(source_ext) -> ".js";
default(module_ext) -> ".min";
default(custom_tags_dir) -> "".

target_file(Source, Options) ->
    SourceExt = option(source_ext, Options),
    ModuleExt = option(module_ext, Options),
    filename:join([option(out_dir, Options), filename:basename(Source, SourceExt) ++ ModuleExt ++ SourceExt]).

compress_each([]) ->
    ok;
compress_each([{Source, Target} | Rest]) ->
    Cmd = lists:flatten(["uglifyjs ", " -o ", Target, " ", Source]),
    ShOpts = [{use_stdout, false}, return_on_error],
    case rebar_utils:sh(Cmd, ShOpts) of
        {ok, _} ->
            ?CONSOLE("Compressed asset ~s to ~s\n", [Source, Target]);
        {error, Reason} ->
            ?ERROR("Compressing asset ~s failed:~n  ~p~n",
                   [Source, Reason]),
            ?ABORT
    end,
    compress_each(Rest).

uglifyjs_is_present() ->
    Cmd = lists:flatten(["which uglifyjs"]),
    ShOpts = [{use_stdout, false}, return_on_error],
    case rebar_utils:sh(Cmd, ShOpts) of
        {ok, _} ->
            true;
        {error, _} ->
            false
    end.
