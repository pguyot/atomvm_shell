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

-module(atomvm_shell).

-export([start/0]).

start() ->
    Creds = maps:get(sta, config:get()),
    case network:wait_for_sta(Creds, 30000) of
        {ok, {Address, _Netmask, _Gateway}} ->
            distribution_start(Address);
        Error ->
            io:format("An error occurred starting network: ~p~n", [Error])
    end.

distribution_start(Address) ->
    {ok, _EPMDPid} = epmd:start_link([]),
    {ok, _KernelPid} = kernel:start(normal, []),
    {X, Y, Z, T} = Address,
    NodeName = maps:get(node_name, config:get()),
    Cookie = maps:get(cookie, config:get()),
    Node = list_to_atom(lists:flatten(io_lib:format("~s@~B.~B.~B.~B", [NodeName, X, Y, Z, T]))),
    {ok, _NetKernelPid} = net_kernel:start(Node, #{name_domain => longnames}),
    io:format("Distribution was started\n"),
    io:format("Node is ~p\n", [node()]),
    net_kernel:set_cookie(Cookie),
    io:format("Cookie is ~s\n", [net_kernel:get_cookie()]),
    register(main, self()),
    io:format("To connect to shell, run:\n"),
    io:format("erl -remsh ~s -setcookie ~s\n", [Node, Cookie]),
    io:format("To stop program, this process is called 'main' and can be stopped from the shell with:\n"),
    io:format("main ! quit.\n"),
    receive
        quit -> ok
    end.
