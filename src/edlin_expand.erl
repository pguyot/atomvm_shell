%
% This file is part of AtomVM.
%
% Copyright 2025 Paul Guyot <pguyot@kallisys.net>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%    http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%
-module(edlin_expand).

-export([expand/2]).

expand(ReverseStr, _Options) ->
    case string:split(ReverseStr, ":", all) of
        [ReverseStr] ->
            {no, [], []};
        [FuncStr, ModuleStr] ->
            try
                Module = list_to_atom(lists:reverse(ModuleStr)),
                case code:ensure_loaded(Module) of
                    {module, Module} ->
                        FuncPrefix = lists:reverse(FuncStr),
                        FuncPrefixLen = length(FuncPrefix),
                        ResultExpand = lists:foldl(
                            fun({Func, _Arity}, {Longest, Candidates} = Acc) ->
                                FuncString = atom_to_list(Func),
                                FuncStringPrefix = lists:sublist(FuncString, FuncPrefixLen),
                                if
                                    FuncStringPrefix =:= FuncPrefix ->
                                        case Longest of
                                            undefined ->
                                                {FuncString, [FuncString]};
                                            _ ->
                                                {longest_prefix(Longest, FuncString), [
                                                    FuncString | Candidates
                                                ]}
                                        end;
                                    true ->
                                        Acc
                                end
                            end,
                            {undefined, []},
                            Module:module_info(exports)
                        ),
                        case ResultExpand of
                            {undefined, _} ->
                                {no, [], []};
                            {Prefix, Candidates} ->
                                {yes, lists:nthtail(FuncPrefixLen, Prefix), [
                                    #{
                                        options => [highlight_all],
                                        title => "functions",
                                        elems => [{Func, [{ending, "("}]} || Func <- Candidates]
                                    }
                                ]}
                        end;
                    {error, _} ->
                        {no, [], []}
                end
            catch
                _:_ ->
                    {no, [], []}
            end
    end.

longest_prefix(Str1, Str2) ->
    longest_prefix(Str1, Str2, []).

longest_prefix([C | T1], [C | T2], Acc) ->
    longest_prefix(T1, T2, [C | Acc]);
longest_prefix(_Str1, _Str2, Acc) ->
    lists:reverse(Acc).
