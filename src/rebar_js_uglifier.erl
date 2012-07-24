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

%% The rebar_js_uglifier module is a plugin for rebar that compresses
%% javascript files using UglifyJS.
%%
%% Configuration options should be placed in rebar.config under
%% 'js_uglify'.  Available options include:
%%
%%  uglify_path: path to the uglify executable
%%               "/usr/local/bin/uglifyjs" by default
%%
%%  doc_root: where to find javascript files to compile
%%            "priv/assets/javascripts" by default
%%
%%  out_dir: where to put compressed javascript files
%%           "priv/www/javascripts" by default
%%
%%  compressions: list of tuples describing each transformation
%%                empty list by default.
%%
%% The default settings are the equivalent of:
%%   {js_uglify, [
%%               {uglify_path, "/usr/local/bin/uglifyjs"},
%%               {doc_root, "priv/assets/javascripts"},
%%               {out_dir, "priv/www/javascripts"},
%%               {compressions, []}
%%              ]}.
%%
%% An example of compressing a series of javascript files:
%%
%%   {js_uglify, [
%%      {uglify_path, "/usr/local/bin/uglifyjs"},
%%      {doc_root, "priv/assets/javascripts"},
%%      {out_dir, "priv/www/javascripts"},
%%      {compressions, [
%%          {"vendor.min.js", "vendor.js"}
%%      ]}
%%   ]}.
%%

-module(rebar_js_uglifier).

-export([compile/2,
         clean/2]).

-include("rebar.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

compile(Config, _AppFile) ->
    Options = options(Config),
    OutDir = option(out_dir, Options),
    DocRoot = option(doc_root, Options),
    Compressions = option(compressions, Options),
    Uglifier = option(uglify_path, Options),
    case uglifyjs_is_present(Uglifier) of
        true ->
            Targets = [{normalize_path(Destination, OutDir),
                        normalize_path(Source, DocRoot)}
                       || {Destination, Source} <- Compressions],
            compress_each(Targets);
        false ->
            ?ERROR(
                "~n===============================================~n"
                " You need to install uglify-js to compress assets~n"
                " Please run the following: npm install uglify-js~n"
                "===============================================~n~n",
                []),
            ?ABORT
    end.

clean(Config, _AppFile) ->
    Options = options(Config),
    OutDir = option(out_dir, Options),
    Compressions = option(compressions, Options),
    Targets = [normalize_path(Destination, OutDir)
               || {Destination, _Source} <- Compressions],
    delete_each(Targets),
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

options(Config) ->
    rebar_config:get(Config, js_uglify, []).

option(Option, Options) ->
    proplists:get_value(Option, Options, default(Option)).

default(doc_root) -> "priv/assets/javascripts";
default(out_dir)  -> "priv/www/javascripts";
default(uglify_path) -> "/usr/local/bin/uglifyjs";
default(compressions) -> [].

normalize_path(Path, Basedir) -> filename:join([Basedir, Path]).

needs_compress(Source, Destination) ->
    filelib:last_modified(Destination) < filelib:last_modified(Source).

delete_each([]) ->
    ok;
delete_each([First | Rest]) ->
    case file:delete(First) of
        ok ->
            ok;
        {error, enoent} ->
            ok;
        {error, Reason} ->
            ?ERROR("Failed to delete ~s: ~p\n", [First, Reason])
    end,
    delete_each(Rest).

compress_each([]) ->
    ok;
compress_each([{Destination, Source} | Rest]) ->
    case needs_compress(Source, Destination) of
        true ->
            Cmd = lists:flatten(["uglifyjs ", " -o ", Destination, " ", Source]),
            ShOpts = [{use_stdout, false}, return_on_error],
            case rebar_utils:sh(Cmd, ShOpts) of
                {ok, _} ->
                    ?CONSOLE("Compressed asset ~s to ~s\n", [Source, Destination]);
                {error, Reason} ->
                    ?ERROR("Compressing asset ~s failed:~n  ~p~n",
                           [Source, Reason]),
                    ?ABORT
            end;
        false ->
            ok
    end,
    compress_each(Rest).

uglifyjs_is_present(Uglifier) -> filelib:is_file(Uglifier).
