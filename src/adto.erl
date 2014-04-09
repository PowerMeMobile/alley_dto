%% @doc Alley DTO interface module.
%% Provides public methods to decode & encode messages.
%% Routes calls by dto record name to suitable dto codec module.

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

%% Funnel dto
module(FunnelDTO) when
            is_record(FunnelDTO, funnel_auth_request_dto)
    orelse  is_record(FunnelDTO, funnel_auth_response_dto)
    orelse  is_record(FunnelDTO, funnel_started_event_dto)
    orelse  is_record(FunnelDTO, funnel_stopped_event_dto)
    orelse  is_record(FunnelDTO, funnel_client_online_event_dto)
    orelse  is_record(FunnelDTO, funnel_client_offline_event_dto)
    orelse  is_record(FunnelDTO, funnel_incoming_sms_dto)
    orelse  is_record(FunnelDTO, funnel_delivery_receipt_dto)
    orelse  is_record(FunnelDTO, funnel_ack_dto)
    orelse  is_record(FunnelDTO, funnel_connections_request_dto)
    orelse  is_record(FunnelDTO, funnel_connections_response_dto) ->
    funnel();

%% Just dto
module(JustDTO) when
            is_record(JustDTO, just_sms_request_dto)
    orelse  is_record(JustDTO, just_sms_response_dto)
    orelse  is_record(JustDTO, just_incoming_sms_dto)
    orelse  is_record(JustDTO, just_delivery_receipt_dto) ->
    just();

%% k1api dto
module(K1apiDTO) when
            is_record(K1apiDTO, k1api_auth_request_dto)
    orelse  is_record(K1apiDTO, k1api_auth_response_dto)
    orelse  is_record(K1apiDTO, k1api_sms_delivery_status_request_dto)
    orelse  is_record(K1apiDTO, k1api_sms_delivery_status_response_dto)
    orelse  is_record(K1apiDTO, k1api_retrieve_sms_request_dto)
    orelse  is_record(K1apiDTO, k1api_retrieve_sms_response_dto)
    orelse  is_record(K1apiDTO, k1api_remove_retrieved_sms_request_dto)
    orelse  is_record(K1apiDTO, k1api_subscribe_incoming_sms_request_dto)
    orelse  is_record(K1apiDTO, k1api_subscribe_incoming_sms_response_dto)
    orelse  is_record(K1apiDTO, k1api_unsubscribe_incoming_sms_request_dto)
    orelse  is_record(K1apiDTO, k1api_unsubscribe_incoming_sms_response_dto)
    orelse  is_record(K1apiDTO, k1api_sms_notification_request_dto)
    orelse  is_record(K1apiDTO, k1api_subscribe_sms_receipts_request_dto)
    orelse  is_record(K1apiDTO, k1api_subscribe_sms_receipts_response_dto)
    orelse  is_record(K1apiDTO, k1api_unsubscribe_sms_receipts_request_dto)
    orelse  is_record(K1apiDTO, k1api_unsubscribe_sms_receipts_response_dto)
    orelse  is_record(K1apiDTO, k1api_sms_delivery_receipt_notification_dto)
    orelse  is_record(K1apiDTO, k1api_coverage_response_dto) ->
    k1api();

module(Type) ->
    erlang:error({adto_unsupported_type, Type}).

%% codec modules
funnel() -> adto_funnel.
just() ->   adto_just.
k1api() ->  adto_k1api.
