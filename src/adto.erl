%% @doc Alley DTO interface module.
%% Provides public methods to decode & encode messages.
%% Routes calls by dto record name to suitable decoder module.

-module(adto).

-export([
	encode/1,
	decode/2
]).

-include("adto.hrl").

%% ===================================================================
%% API Functions
%% ===================================================================

-spec encode(message_type_dto()) ->
	{ok, binary()} |
	{error, Reason :: any()}.
encode(Message) ->
	Module = module(Message),
	Module:encode(Message).

-spec decode(message_type_dto(), binary()) ->
	{ok, message_type_dto()} |
	{error, Reason :: any()}.
decode(Type, Message) ->
	Module = module(Type),
	Module:decode(Type, Message).

%% ===================================================================
%% Internal Functions
%% ===================================================================

%% Funnel entities
module(#funnel_auth_request_dto{}) ->
	funnel();
module(#funnel_auth_response_dto{}) ->
	funnel();
module(#funnel_started_event_dto{}) ->
	funnel();
module(#funnel_stopped_event_dto{}) ->
	funnel();
module(#funnel_client_online_event_dto{}) ->
	funnel();
module(#funnel_client_offline_event_dto{}) ->
	funnel();
module(#funnel_incoming_sms_dto{}) ->
	funnel();
module(#funnel_delivery_receipt_dto{}) ->
	funnel();
module(#funnel_ack_dto{}) ->
	funnel();

%% Just entities
module(#just_sms_request_dto{}) ->
	just();
module(#just_sms_response_dto{}) ->
	just();
module(#just_incoming_sms_dto{}) ->
	just();
module(#just_delivery_receipt_dto{}) ->
	just();

%% k1api entities
module(#k1api_sms_delivery_status_request_dto{}) ->
	k1api();
module(#k1api_sms_delivery_status_response_dto{}) ->
	k1api();

%% invoke error if type unsupported
module(Type) ->
	erlang:error({adto_unsupported_type, Type}).


funnel() -> adto_funnel.

just() -> adto_just.

k1api() -> adto_k1api.
