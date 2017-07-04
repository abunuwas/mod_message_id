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
-include("ejabberd.hrl").
-include("fxml.hrl").
-include("xmpp_codec.hrl").
-include("jid.hrl").

%% gen_mod API callbacks
-export([start/2, stop/1, message_id_hook/1, offline_message_id_hook/4]).

-ifndef(LAGER).
-define(LAGER, 1).
-endif.


start(_Host, _Opt) -> 
    ejabberd_hooks:add(filter_packet, global, ?MODULE, message_id_hook, 1),
    ejabberd_hooks:add(unset_presence_hook, _Host, ?MODULE, offline_message_id_hook, 1),
    ?INFO_MSG("Registering message_id...", []),
    ok.

stop(_Host) -> 
    ejabberd_hooks:delete(filter_packet, global, ?MODULE, message_id_hook, 1),
    ejabberd_hooks:delete(unset_presence_hook, _Host, ?MODULE, offline_message_id_hook, 1),
    ?INFO_MSG("Deregistering message_id_hook...", []),
    ok.

update_user_message_id(Username) ->
    {ok, C} = eredis:start_link(),
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
    case addressed_to_platform(To) of
        false -> false;
        true ->
            case XML#xmlel.name of
                <<"presence">> -> can_modify_presence(XML);
                <<"iq">> -> can_modify_iq(XML);
                <<"message">> -> can_modify_message(XML);
                <<"error">> -> can_modify_error(XML)
            end
    end.
    

addressed_to_platform(To) ->
    ?INFO_MSG(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> To: ~p~n", [To]),
    Pos = string:rstr(erlang:binary_to_list(To#jid.server), "component"),
    if
        Pos >= 1 
            -> true;
        true -> false
    end.


can_modify_presence(XML) -> 
    case fxml:get_tag_attr(<<"type">>, XML) of
        false -> true;
        <<"available">> -> true;
        <<"unavailable">> -> false;
        <<"account-created">> -> true;
        _ -> false
    end.


can_modify_iq(XML) ->
    case fxml:get_tag_attr(<<"type">>, XML) of
        <<"error">> -> can_modify_error(XML);
        _ -> inspect_iq_children(XML)
    end.


inspect_iq_children(XML) ->
    Children = XML#xmlel.children,
    case Children of
        [] -> false
    end,
    case Children#xmlel.name of
        <<"ping">> -> fase;
        <<"echo">> -> false;
        <<"query">> -> false;
        true -> true
    end.


can_modify_message(XML) ->
    true.


can_modify_error({ Name, Attrs, Children } = XML) ->
    case Name of 
        <<"iq">> -> Children#xmlel.name == <<"intamacstream">> andalso fxml:get_tag_attr(<<"type">>, Children) == <<"start">>;
        _ -> false
    end.

    
%% Creates a new packet with a user-specific increasing
%% sequence ID. The sequence number is assigned to a top
%% level attribute of the stanza called ejab_seq. 
create_new_packet({From, To, XML} = Packet) ->
    Username = erlang:binary_to_list(From#jid.user),
    NewValue = update_user_message_id(Username),
    AttrsWithSequence = lists:merge(XML#xmlel.attrs, [{<<"ejab_seq">>, erlang:list_to_binary(erlang:integer_to_list(NewValue))}]),
    NewXML = #xmlel{ name = XML#xmlel.name, attrs = AttrsWithSequence, children = XML#xmlel.children },
    NewPacket = {From, To, NewXML},
    NewPacket.


message_id_hook({From, To, XML} = Packet) ->
    ?INFO_MSG("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++", []),
    ?INFO_MSG("Packet filtered: ~p~n", 
        [XML]),
    case can_modify(Packet) of
        true ->
            ?INFO_MSG("-------------------------------------------------------------------------------------------------------We are going to modify: ~p~n", 
                [XML]),
            NewPacket = create_new_packet(Packet),
            NewPacket;
        false -> Packet
    end.


offline_message_id_hook(User, Server, Resource, Status) ->
    ?INFO_MSG("*****************************************************************************************************************", []),
    From = jid:make(User, Server, Resource),
    To = jid:make(<<"user">>, <<"component.use-xmpp-01">>, <<"">>),
    NewValue = update_user_message_id(User),
    OfflinePacket = {xmlel, <<"presence">>, [{<<"type">>, <<"unavailable">>}, {<<"ejab_seq">>, erlang:list_to_binary(erlang:integer_to_list(NewValue))}], [] },
    ?INFO_MSG("Built offline packet: ~p~n", [OfflinePacket]),
    ejabberd_router:route(From, To, OfflinePacket),
    {User, Server, Resource, Status}.
