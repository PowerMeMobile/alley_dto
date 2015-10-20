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
    orelse is_record(FunnelDTO, funnel_auth_response_dto)
    orelse is_record(FunnelDTO, funnel_started_event_dto)
    orelse is_record(FunnelDTO, funnel_stopped_event_dto)
    orelse is_record(FunnelDTO, funnel_client_online_event_dto)
    orelse is_record(FunnelDTO, funnel_client_offline_event_dto)
    orelse is_record(FunnelDTO, funnel_incoming_sms_dto)
    orelse is_record(FunnelDTO, funnel_delivery_receipt_dto)
    orelse is_record(FunnelDTO, funnel_ack_dto)
    orelse is_record(FunnelDTO, funnel_connections_request_dto)
    orelse is_record(FunnelDTO, funnel_connections_response_dto) ->
    funnel();

%% Just dto
module(JustDTO) when
           is_record(JustDTO, just_sms_request_dto)
    orelse is_record(JustDTO, just_sms_response_dto)
    orelse is_record(JustDTO, just_incoming_sms_dto)
    orelse is_record(JustDTO, just_delivery_receipt_dto) ->
    just();

%% common dto
module(DTO) when
           is_record(DTO, error_resp_v1)
    orelse is_record(DTO, sms_req_v1)
    orelse is_record(DTO, auth_req_v2)
    orelse is_record(DTO, auth_resp_v2)
    orelse is_record(DTO, auth_req_v3)
    orelse is_record(DTO, auth_resp_v3)
    orelse is_record(DTO, sms_status_req_v1)
    orelse is_record(DTO, sms_status_resp_v1)
    orelse is_record(DTO, credit_req_v1)
    orelse is_record(DTO, credit_resp_v1)
    orelse is_record(DTO, blacklist_req_v1)
    orelse is_record(DTO, blacklist_resp_v1)
    orelse is_record(DTO, coverage_req_v1)
    orelse is_record(DTO, coverage_resp_v1)
    orelse is_record(DTO, block_req_v1)
    orelse is_record(DTO, block_resp_v1)
    orelse is_record(DTO, unblock_req_v1)
    orelse is_record(DTO, unblock_resp_v1)
    orelse is_record(DTO, inbox_req_v1)
    orelse is_record(DTO, inbox_resp_v1)
    orelse is_record(DTO, retrieve_incoming_req_v1)
    orelse is_record(DTO, retrieve_incoming_resp_v1)
    orelse is_record(DTO, connections_req_v1)
    orelse is_record(DTO, connections_resp_v1)
    orelse is_record(DTO, disconnect_req_v1)
    orelse is_record(DTO, disconnect_resp_v1)
    orelse is_record(DTO, throughput_req_v1)
    orelse is_record(DTO, throughput_resp_v1)
    orelse is_record(DTO, gateway_states_req_v1)
    orelse is_record(DTO, gateway_states_resp_v1)
    orelse is_record(DTO, gateway_state_req_v1)
    orelse is_record(DTO, gateway_state_resp_v1)
    orelse is_record(DTO, start_gateway_req_v1)
    orelse is_record(DTO, start_gateway_resp_v1)
    orelse is_record(DTO, stop_gateway_req_v1)
    orelse is_record(DTO, stop_gateway_resp_v1)
    orelse is_record(DTO, sub_incoming_sms_req_v1)
    orelse is_record(DTO, sub_incoming_sms_resp_v1)
    orelse is_record(DTO, unsub_incoming_sms_req_v1)
    orelse is_record(DTO, unsub_incoming_sms_resp_v1)
    orelse is_record(DTO, incoming_sms_notification_v1)
    orelse is_record(DTO, sub_sms_receipts_req_v1)
    orelse is_record(DTO, sub_sms_receipts_resp_v1)
    orelse is_record(DTO, unsub_sms_receipts_req_v1)
    orelse is_record(DTO, unsub_sms_receipts_resp_v1)
    orelse is_record(DTO, sms_receipt_notification_v1)
     ->
    common();

module(Type) ->
    erlang:error({unknown_adto_type, Type}).

%% codec modules
funnel() -> adto_funnel.
just()   -> adto_just.
common() -> adto_common.
