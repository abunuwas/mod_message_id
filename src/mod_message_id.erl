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
%%-include("fxml.hrl").
-include("jlib.hrl").

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

update_user_message_id(Username) ->
    {ok, C} = eredis:start_link(),
    ?INFO_MSG("Connection to Redis: ~p~n", [C]),
    {ok, Last} = eredis:q(C, ["GET", Username]),
    case Last of
        undefined -> 
            {ok, <<"OK">>} = eredis:q(C, ["SET", Username, 1]),
            1;
        _ ->
            LastInteger = erlang:list_to_integer(erlang:binary_to_list(Last)),
            NewValue = LastInteger + 1,
            {ok, <<"OK">>} = eredis:q(C, ["SET", Username, NewValue]),
            NewValue
    end.


can_modify({From, To, XML} = Packet) ->
    Pos = string:rstr(erlang:binary_to_list(To), "component"),
    if
        Pos  > 1 ->
            true;
    else -> false
    end.

    
%% Creates a presence stanza with custom attribute msg="account-created"
%% when a user craetes an account with the server.
create_new_packet({From, To, XML} = Packet) ->
    Username = erlang:binary_to_list(From#jid.user),
    NewValue = update_user_message_id(Username),
    ?INFO_MSG("New message id for this user: ~p~n", [NewValue]),
    SequenceAttr = lists:merge(XML#xmlel.attrs, [{<<"ejab_seq">>, erlang:list_to_binary(erlang:integer_to_list(NewValue))}]),
    ?INFO_MSG("###################################### Attrs modified: ~p~n", [SequenceAttr]),
    NewXML = #xmlel{ name = XML#xmlel.name, attrs = SequenceAttr, children = XML#xmlel.children },
    NewPacket = {From, To, NewXML},
    NewPacket.
    %%Sequence = #xmlel{ name = <<"ejab_seq">>, children = [{xmlcdata, <<"HAAAAAAAAAAAAAAAAAAAAAAAAAA">>}]},
    %%NewPacket = Packet#xmlel{ children = Sequence },
    %%NewPacket.

message_id_hook({From, To, XML} = Packet) ->
    ?INFO_MSG("Packet filtered: ~p~n", 
        [From#jid.user]),
    case can_modify(Packet) of
        true ->
            NewPacket = create_new_packet(Packet),
            NewPacket;
        false -> Packet
    end.


