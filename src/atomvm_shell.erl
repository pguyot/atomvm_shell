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

-include_lib("kernel/include/logger.hrl").

-export([start/0]).

-define(LOG_CFG, info).

start() ->
    {ok, _} = logger_manager:start_link(#{log_level => ?LOG_CFG}),
    STAcfg = maps:get(sta, config:get()),
    MDNSCfg = maps:get(mdns, config:get(), []),
    Start = self(),
    NetworkCfg0 = [
        {sta,
            lists:flatten([
                {got_ip, fun(IpInfo) -> Start ! IpInfo end}
                | STAcfg
            ])},
        {sntp,
            lists:flatten([
                {synchronized, fun(_) -> logger:notice("Time synchronized by SNTP.") end}
                | maps:get(sntp, config:get(), [])
            ])}
    ],
    NetworkCfg =
        case MDNSCfg of
            [] -> NetworkCfg0;
            _ -> [{mdns, MDNSCfg} | NetworkCfg0]
        end,
    logger:info("Starting network with config: ~p", [sanitize_netcfg(NetworkCfg)]),
    case network:start(NetworkCfg) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            ?LOG_ERROR("An error occurred starting network: ~p", [Reason]),
            error(Reason);
        Other ->
            ?LOG_ERROR("An error occurred starting network: ~p", [Other]),
            error(Other)
    end,

    IP =
        receive
            {A, _M, _G} = _IpInfo -> A
        after 30_000 ->
            ?LOG_ERROR("No network after 30 seconds. Aborting!"),
            error(no_network)
        end,
    DHCPHostname = proplists:get_value(dhcp_hostname, STAcfg),
    MDNSHostname = proplists:get_value(hostname, MDNSCfg),
    Hostname =
        if
            DHCPHostname =/= undefined -> DHCPHostname;
            MDNSHostname =/= undefined -> list_to_binary([MDNSHostname, ".local"]);
            true -> IP
        end,
    ?LOG_DEBUG("Starting distribution with hostname address: ~p", [Hostname]),
    distribution_start(Hostname).

distribution_start(Hostname) ->
    {ok, _EPMDPid} = epmd:start_link([]),
    {ok, _KernelPid} = kernel:start(normal, []),
    NodeName = maps:get(node_name, config:get()),
    Cookie = maps:get(cookie, config:get()),
    Node = mk_node(NodeName, Hostname),
    {ok, _NetKernelPid} = net_kernel:start(Node, #{name_domain => longnames}),
    logger:notice("Distribution was started"),
    logger:notice("Node is ~p", [node()]),
    net_kernel:set_cookie(Cookie),
    logger:notice("Cookie is ~s", [net_kernel:get_cookie()]),
    register(main, self()),
    io:format("\n\tTo connect to shell, run:\n"),
    io:format("\terl -remsh ~s -setcookie ~s\n", [Node, Cookie]),
    io:format(
        "\tTo stop program, this process is called 'main' and can be stopped from the shell with:\n"
    ),
    io:format("\tmain ! quit.\n\n"),
    receive
        quit -> ok
    end.

mk_node(NodeName, NameOrIP) when is_list(NameOrIP) ->
    list_to_atom(lists:flatten(io_lib:format("~s@~s", [NodeName, NameOrIP])));
mk_node(NodeName, NameOrIP) when is_binary(NameOrIP) ->
    list_to_atom(lists:flatten(io_lib:format("~s@~s", [NodeName, NameOrIP])));
mk_node(NodeName, NameOrIP) when is_tuple(NameOrIP) ->
    {X, Y, Z, T} = NameOrIP,
    list_to_atom(lists:flatten(io_lib:format("~s@~B.~B.~B.~B", [NodeName, X, Y, Z, T])));
mk_node(_NodeName, NameOrIP) ->
    ?LOG_ERROR("Fatal error, ~p is not a valid ip() or hostname string() or binary()", [NameOrIP]),
    error({invalid, NameOrIP}).

sanitize_netcfg(NetworkCfg) ->
    Sta = proplists:get_value(sta, NetworkCfg),
    SafeSTA = lists:flatten([proplists:delete(psk, Sta), {psk, "********"}]),
    Tlist = proplists:delete(sta, NetworkCfg),
    Sanitized = [{sta, lists:flatten([SafeSTA | Tlist])}],
    ?LOG_DEBUG("~n\tConfig: ~p~n\tSanitized: ~p", [NetworkCfg, Sanitized]),
    Sanitized.
