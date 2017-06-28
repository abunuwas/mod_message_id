%%%----------------------------------------------------------------------
%%% File    : mod_register_announce.erl
%%% Author  : Jose Haro Peralta <jharoperalta@intamac.com>
%%% Purpose : Announce user registration to platform. 
%%% Created : 13 April 2017 by Jose Haro Peralta <jharoperalta@intamac.com>
%%%
%%% ----------------------------------------------------------------------
%%% Description: 
%%% This module implements a hook on the event register_user
%%% raised by Ejabberd whenever a new user creates an account
%%% with the server. This event is raised immediately after 
%%% registration. The custom hook creates a presence stanza
%%% on behalf of the registered user to announce the event
%%% to the platform. 
%%% ----------------------------------------------------------------------
%%%
%%% Copyright (C) Intamac Systems Ltd.
%%%
%%%----------------------------------------------------------------------


-module(mod_message_id).

%% Required by ?INFO_MSG macros
-include("logger.hrl").

%% gen_mod API callbacks
-export([start/2, stop/1, message_id/2]).

-ifndef(LAGER).
-define(LAGER, 1).
-endif.

start(_Host, _Opt) -> 
    ejabberd_hooks:add(filter_packet_hook, _Host, ?MODULE, message_id, 10),
    %%ejabberd_hooks:add(register_user, <<"xmpp.sprue.intamac.com">>, ?MODULE, register_announce_hook, 10),
    ?INFO_MSG("Registering message_id...", []),
    ok.

stop(_Host) -> 
    ejabberd_hooks:delete(filter_packet_hook, _Host, ?MODULE, message_id, 10),
    ?INFO_MSG("Deregistering message_id_hook...", []),
    ok.

%% Creates a presence stanza with custom attribute msg="account-created"
%% when a user craetes an account with the server.
announce_registration(User, Server) ->
    Packet = {xmlel,
                 <<"presence">>,
                 [{<<"type">>,<<"account-created">>}],
                 []
                 },
    To = jid:make(<<>>, <<"component.sprue">>, <<>>),
    From = jid:make(User, Server, <<"">>),
    ejabberd_router:route(From, To, Packet).

message_id_hook(Packet) ->
    ?INFO_MSG("mod_register_announce:register_announce_hook: 
    	An account has been created for the following user: ~p~n", 
        Packet),
    Packet.


