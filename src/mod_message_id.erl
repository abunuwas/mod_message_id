%%%----------------------------------------------------------------------
%%% File    : mod_message_id.erl
%%% Author  : Jose Haro Peralta <jharoperalta@intamac.com>
%%% Purpose : Add sequencing and timestamp to user-specific messages. 
%%% Created : 13 April 2017 by Jose Haro Peralta <jharoperalta@intamac.com>
%%%
%%% ----------------------------------------------------------------------
%%% Description: 
%%% This module implements a hook that filters out packets leaving 
%%% the server, and adds a sequencing number as well as a timestamp
%%% if they are being addressed to the XMPP Component. It also creates
%%% a custom offline stanza with the same attributes so that the platform
%%% can be made aware of the order in which they were generated. 
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
-export([start/2, stop/1, message_id_hook/1, offline_message_id_hook/4, update_user_message_id/1]).

-ifndef(LAGER).
-define(LAGER, 1).
-endif.


%% Add custom callbacks for filter_packet and unet_prsence_hook events.
start(_Host, _Opt) -> 
    ejabberd_hooks:add(filter_packet, global, ?MODULE, message_id_hook, 1),
    ejabberd_hooks:add(unset_presence_hook, _Host, ?MODULE, offline_message_id_hook, 1),
    ?INFO_MSG("Registering message_id...", []),
    ok.

%% Remove custom callbacks. 
stop(_Host) -> 
    ejabberd_hooks:delete(filter_packet, global, ?MODULE, message_id_hook, 1),
    ejabberd_hooks:delete(unset_presence_hook, _Host, ?MODULE, offline_message_id_hook, 1),
    ?INFO_MSG("Deregistering message_id_hook...", []),
    ok.


%% Returns the next sequence number for a given user.
%% At the moment this uses only Redis to manage this
%% information, however this functionality can be
%% decoupled to use a different storage system if so desired.
update_user_message_id(Username) ->
    {ok, C} = eredis:start_link("stgejabberdswann.bndouw.ng.0001.euw1.cache.amazonaws.com", 6379, 0),
    {ok, Last} = eredis:q(C, ["GET", Username]),
    case Last of
        undefined -> 
            {ok, <<"OK">>} = eredis:q(C, ["SET", Username, 0]),
            0;
        _ ->
            LastInteger = erlang:list_to_integer(erlang:binary_to_list(Last)),
            if LastInteger 
                >= 1000 ->
                    NewValue = 0,
                    {ok, <<"OK">>} = eredis:q(C, ["SET", Username, NewValue]),
                    NewValue;
                true ->
                    NewValue = LastInteger + 1,
                    {ok, <<"OK">>} = eredis:q(C, ["SET", Username, NewValue]),
                    NewValue
            end
    end.


%% Checks whether a packet should be modified. 
can_modify({From, To, XML} = Packet) ->
    case addressed_to_platform(To) of
        false -> false;
        true ->
            case XML#xmlel.name of
                <<"presence">> -> can_modify_presence(XML);
                <<"iq">> -> can_modify_iq(XML);
                <<"message">> -> can_modify_message(XML);
                _ -> false
            end
    end.
    

%% Only stanzas addressed to the platform XMPP Component
%% should be modified. 
addressed_to_platform(To) ->
    Pos = string:rstr(erlang:binary_to_list(To#jid.server), "component"),
    if
        Pos >= 1 
            -> true;
        true -> false
    end.


%% Determines whether presence stanzas can be modified. 
can_modify_presence(XML) -> 
    case fxml:get_tag_attr(<<"type">>, XML) of
        false -> true;
        {_, <<"available">>} -> true;
        {_, <<"unavailable">>} -> false;
        {_, <<"account-created">>} -> true;
        _ -> false
    end.


%% Determines whether <iq> stanzas can be modified. 
can_modify_iq(XML) ->
    case fxml:get_tag_attr(<<"type">>, XML) of
        {_,<<"error">>} -> can_modify_error(XML);
        _ -> inspect_iq_children(XML)
    end.


%% Determines whether <iq> stanzas can
%% be modified by inspecting their children. 
inspect_iq_children(XML) ->
    Children = XML#xmlel.children,
    case Children of
        [] -> false;
        _ ->
            FirstChild = lists:nth(1, Children),
            case FirstChild#xmlel.name of
                <<"ping">> -> false;
                <<"echo">> -> false;
                <<"query">> -> false;
                _ -> true
            end
    end.


%% All <message> stanzas can be modified.
can_modify_message(XML) ->
    true.


%% Determines whether an error stanza can be modified.
%% Only intamacstream stanzas should be returned to 
%% the platform with a sequence number, since in case
%% of error they still need to be marked as complete in 
%% the database. 
can_modify_error(XML) ->
    case XML#xmlel.name of 
        <<"iq">> -> 
            Children = XML#xmlel.children,
            case Children of
                [] -> false;
                _ ->
                    FirstChild = lists:nth(1, Children),
                    case FirstChild#xmlel.name of
                        <<"intamacstream">> -> true;
                        _ -> false
                    end
            end
    end.


%% Returns a timestamp in unix time down to the second.
%% now/0 is not warp safe, however in Erlang OPT/17 there are
%% no other choices. If an upgrade is made to Erlang OPT/18,
%% then the new API erlang:timestamp/0 should be preferred.  
get_timestamp() ->
    {MegaSeconds, Seconds, _} = erlang:now(),
    Timestamp = MegaSeconds * 1000000 + Seconds,
    Timestamp.


%% Creates a new packet with a user-specific increasing
%% sequence ID. The sequence number is assigned to a top
%% level attribute of the stanza called ejab_sequence. It
%% also includes a timestamp under custom attribute ejab_timestamp. 
create_new_packet({From, To, XML} = Packet) ->
    Username = erlang:binary_to_list(From#jid.user),
    NewValue = update_user_message_id(Username),
    AttrsWithSequence = lists:merge(XML#xmlel.attrs, [{<<"ejab_sequence">>, erlang:list_to_binary(erlang:integer_to_list(NewValue))},
                                                      {<<"ejab_timestamp">>, erlang:list_to_binary(erlang:integer_to_list(get_timestamp()))}]),
    NewXML = #xmlel{ name = XML#xmlel.name, attrs = AttrsWithSequence, children = XML#xmlel.children },
    NewPacket = {From, To, NewXML},
    NewPacket.


%% Checks whether packets should be modified. If so,
%% it returns the modified packet, otherwise returns
%% the same packet. 
message_id_hook({From, To, XML} = Packet) ->
    case can_modify(Packet) of
        true ->
            ?INFO_MSG("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++", []),
            ?INFO_MSG("The following packet will be modified: ~p~n", [Packet]),
            NewPacket = create_new_packet(Packet),
            NewPacket;
        false -> Packet
    end.


%% Creates an offline message on behalf of the user disconnecting,
%% and sends it to the platform with the expected attributes: 
%% ejab_sequence and ejab_timestamp.
offline_message_id_hook(User, Server, Resource, Status) ->
    ?INFO_MSG("***************************************************************************************************************************", []),
    From = jid:make(User, Server, Resource),
    To = jid:make(<<"user">>, <<"component.use-xmpp-01">>, <<"">>),
    NewValue = update_user_message_id(User),
    OfflinePacket = {xmlel, <<"presence">>, [{<<"type">>, <<"unavailable">>}, 
                                             {<<"ejab_sequence">>, erlang:list_to_binary(erlang:integer_to_list(NewValue))},
                                             {<<"ejab_timestamp">>, erlang:list_to_binary(erlang:integer_to_list(get_timestamp()))}], 
                                            [] },
    ?INFO_MSG("The following offline packet was created: ~p~n", [OfflinePacket]),
    ejabberd_router:route(From, To, OfflinePacket),
    {User, Server, Resource, Status}.
