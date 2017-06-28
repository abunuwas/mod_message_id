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
-export([start/2, stop/1, message_id_hook/1]).

-ifndef(LAGER).
-define(LAGER, 1).
-endif.

start(_Host, _Opt) -> 
    ejabberd_hooks:add(filter_packet, global, ?MODULE, message_id_hook, 1),
    %%ejabberd_hooks:add(register_user, <<"xmpp.sprue.intamac.com">>, ?MODULE, register_announce_hook, 10),
    ?INFO_MSG("Registering message_id...", []),
    ok.

stop(_Host) -> 
    ejabberd_hooks:delete(filter_packet, global, ?MODULE, message_id_hook, 1),
    ?INFO_MSG("Deregistering message_id_hook...", []),
    ok.

%% Creates a presence stanza with custom attribute msg="account-created"
%% when a user craetes an account with the server.
create_new_packet({From, To, XML} = Packet) ->
    %% The below works:
    %%Sequence = [{xmlcdata, <<"HAAAAAAAAAAAAAAAAAAAAAAAAAA">>}],
    %%NewPacket = {From, To, fxml:append_subtags(XML, Sequence)},
    %%NewPacket.
    Sequence = #xmlel{ name = <<"ejab_seq">>, children = [{xmlcdata, <<"HAAAAAAAAAAAAAAAAAAAAAAAAAA">>}]},
    NewPacket = Packet#xmlel{ children = Sequence },
    NewPacket.

message_id_hook({From, To, XML} = Packet) ->
    ?INFO_MSG("Packet filtered: ~p~n", 
        [XML]),
    NewPacket = create_new_packet(Packet),
    NewPacket.


