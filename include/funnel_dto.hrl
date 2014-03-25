-ifndef(adto_funnel_hrl).
-define(adto_funnel_hrl, included).

-include("adto_types.hrl").

-record(fun_precise_time_dto, {
    time                :: utc_time(),
    milliseconds        :: integer()
}).

%% ===================================================================
%% Funnel Auth Request
%% ===================================================================

-record(funnel_auth_request_dto, {
    connection_id       :: uuid_dto(),
    ip                  :: binary(), %% <<"127.0.0.1">>
    customer_id         :: binary(), %% <<"system-id">>
    user_id             :: binary(), %% <<"user">>
    password            :: binary(), %% <<"password">>
    type                :: smpp_type_dto(),
    is_cached           :: boolean(),
    timestamp           :: #fun_precise_time_dto{},
    expiration          :: #fun_precise_time_dto{}
}).

%% ===================================================================
%% Funnel Auth Response
%% ===================================================================

-record(funnel_auth_response_customer_dto, {
    id                  :: binary(), %% <<"system-id">>
    uuid                :: uuid_dto(),
    priority            :: integer(),
    rps                 :: integer() | undefined,
    allowed_sources     :: [addr()],
    default_source      :: addr() | undefined,
    networks            :: [#network_dto{}],
    providers           :: [#provider_dto{}],
    default_provider_id :: uuid_dto() | undefined,
    receipts_allowed    :: boolean(),
    no_retry            :: boolean(),
    default_validity    :: binary(), %% <<"000003000000000R">>
    max_validity        :: integer(),   %% in seconds (relative)
    billing_type        :: billing_type_dto()
}).

-type funnel_auth_response_result() ::
    {customer, #funnel_auth_response_customer_dto{}} |
    {error, binary()}.

-record(funnel_auth_response_dto, {
    connection_id :: uuid_dto(),
    result        :: funnel_auth_response_result()
}).

%% ===================================================================
%% Funnel Events
%% ===================================================================

-record(funnel_started_event_dto, {
    timestamp       :: utc_time()
}).

-record(funnel_stopped_event_dto, {
    timestamp       :: utc_time()
}).

-record(funnel_client_online_event_dto, {
    connection_id   :: uuid_dto(),
    customer_id     :: binary(), %% <<"system_id">>
    user_id         :: binary(), %% <<"user_id>>
    type            :: smpp_type_dto(),
    connected_at    :: utc_time(),
    timestamp       :: utc_time()
}).

-record(funnel_client_offline_event_dto, {
    connection_id   :: uuid_dto(),
    customer_id     :: binary(), %% <<"system_id">>
    user_id         :: binary(), %% <<"user_id">>
    type            :: smpp_type_dto(),
    connected_at    :: utc_time(),
    msgs_received   :: integer(),
    msgs_sent       :: integer(),
    errors          :: [#error_dto{}],
    reason          :: normal | closed | unbound | other,
    timestamp       :: utc_time()
}).

%% ===================================================================
%% Funnel Incoming Sms
%% ===================================================================

-type funnel_incoming_sms_datacoding() ::
    gsm0338 |
    ucs2 |
    integer().

-record(funnel_incoming_sms_message_dto, {
    source          :: addr(),
    dest            :: addr(),
    message         :: binary(),
    data_coding     :: funnel_incoming_sms_datacoding()
}).

-record(funnel_incoming_sms_dto, {
    id              :: uuid_dto(),
    messages        :: [#funnel_incoming_sms_message_dto{}]
}).

%% ===================================================================
%% Funnel Delivery Receipt
%% ===================================================================

-type message_state_dto() ::
    delivered |
    expired |
    deleted |
    undeliverable |
    accepted |
    unknown |
    rejected.

-record(funnel_delivery_receipt_container_dto, {
    message_id      :: binary(), %% <<"614">>
    submit_date     :: integer(),  %% utc unix epoch
    done_date       :: integer(), %% utc unix epoch
    message_state   :: message_state_dto(),
    source          :: addr(),
    dest            :: addr()
}).

-record(funnel_delivery_receipt_dto, {
    id              :: uuid_dto(),
    receipts        :: [#funnel_delivery_receipt_dto{}]
}).

%% ===================================================================
%% Funnel Ack
%% ===================================================================

-record(funnel_ack_dto, {
    id              :: uuid_dto()
}).

%% ===================================================================
%% Funnel Connections
%% ===================================================================

-record(funnel_connections_request_dto, {
}).

-record(funnel_connection_dto, {
    connection_id   :: uuid_dto(),
    remote_ip       :: binary(), %% <<"127.0.0.1">>
    customer_id     :: binary(), %% system (smpp) id
    user_id         :: binary(),
    connected_at    :: binary(),
    type            :: smpp_type_dto(),
    msgs_received   :: integer(),
    msgs_sent       :: integer(),
    errors          :: [#error_dto{}]
}).

-record(funnel_connections_response_dto, {
    connections = [#funnel_connection_dto{}]
}).

-type funnel_dto() ::
    #funnel_auth_request_dto{} |
    #funnel_auth_response_dto{} |
    #funnel_started_event_dto{} |
    #funnel_stopped_event_dto{} |
    #funnel_client_online_event_dto{} |
    #funnel_client_offline_event_dto{} |

    #funnel_incoming_sms_dto{} |
    #funnel_delivery_receipt_dto{} |
    #funnel_ack_dto{}.

-endif. % adto_funnel_hrl
