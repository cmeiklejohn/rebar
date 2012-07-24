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

%% The rebar_js_concatenator module is a plugin for rebar that concatenates
%% javascript files.
%%
%% Configuration options should be placed in rebar.config under
%% 'js_concat'.  Available options include:
%%
%%  doc_root: where to find javascript files to concatenate
%%            "priv/assets/javascripts" by default
%%
%%  out_dir: where to put concatenated javascript files
%%           "priv/assets/javascripts" by default
%%
%%  concatenations: list of proplists describing each transformation
%%                  empty list by default
%%
%% The default settings are the equivalent of:
%%   {js_concat, [
%%                {out_dir,    "priv/assets/javascripts"},
%%                {doc_root,   "priv/assets/javascripts"},
%%                {concatenations, []}
%%               ]}.
%%
%% An example of compiling a series of javascript files:
%%
%%   {js_concat, [
%%       {out_dir, "priv/assets/javascripts"},
%%       {doc_root, "priv/assets/javascripts"},
%%       {concatenations, [
%%           [{sources, ["models.js", "controllers.js"]}, {destination, "application.js"}]
%%           [{sources, ["jquery.js", "ember.js"]}, {destination, "vendor.js"}]
%%       ]}
%%   ]}.
%%

-module(rebar_js_concatenator).

-export([compile/2,
         clean/2]).

-include("rebar.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

compile(Config, _AppFile) ->
    Options = options(Config),
    Concatenations = option(concatenations, Options),
    OutDir = option(out_dir, Options),
    DocRoot = option(doc_root, Options),
    Targets = [[{sources, normalize_paths(Sources, DocRoot)},
                {destination, normalize_path(Destination, OutDir)}] ||
              [{sources, Sources}, {destination, Destination}] <- Concatenations],
    build_each(Targets).

clean(Config, _AppFile) ->
    Options = options(Config),
    Concatenations = option(concatenations, Options),
    OutDir = option(out_dir, Options),
    Targets = [normalize_path(Destination, OutDir) ||
              [{sources, _Sources}, {destination, Destination}] <- Concatenations],
    delete_each(Targets),
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

options(Config) ->
    rebar_config:get(Config, js_concat, []).

option(Option, Options) ->
    proplists:get_value(Option, Options, default(Option)).

default(doc_root) -> "priv/assets/javascripts";
default(out_dir)  -> "priv/assets/javascripts";
default(concatenations) -> [].

normalize_paths(Paths, Basedir) ->
    lists:foldl(fun(X, Acc) -> [normalize_path(X, Basedir) | Acc] end, [], Paths).
normalize_path(Path, Basedir) ->
    filename:join([Basedir, Path]).

build_each([]) ->
    ok;
build_each([First | Rest]) ->
    Sources = proplists:get_value(sources, First),
    Destination = proplists:get_value(destination, First),
    Contents = [read(Source) || Source <- Sources],
    case file:write_file(Destination, lists:flatten(Contents), [write]) of
        ok ->
            ?CONSOLE("Built asset ~s~n", [Destination]);
        {error, Reason} ->
            ?ERROR("Building asset ~s failed:~n  ~p~n",
                   [Destination, Reason]),
            ?ABORT
    end,
    build_each(Rest).

read(File) ->
    case file:read_file(File) of
        {ok, Binary} ->
            Binary;
        {error, Reason} ->
            ?ERROR("Reading asset ~s failed during concatenation:~n  ~p~n",
                   [File, Reason]),
            ?ABORT
    end.

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
