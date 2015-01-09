-ifndef(common_dto_hrl).
-define(common_dto_hrl, included).

-include("adto_types.hrl").

%% ===================================================================
%% Authentication
%% ===================================================================

-record(auth_req_v1, {
    req_id          :: uuid_dto(),
    customer_id     :: binary(),
    user_id         :: binary(),
    password        :: binary(),
    connection_type :: binary()
}).

-record(auth_customer_v1, {
    customer_uuid       :: uuid_dto(),
    customer_id         :: binary(),
    user_id             :: binary(),
    pay_type            :: pay_type_dto(),
    allowed_sources     :: [addr()],
    default_source      :: addr() | undefined,
    networks            :: [network_dto()],
    providers           :: [provider_dto()],
    default_provider_id :: uuid_dto() | undefined,
    receipts_allowed    :: boolean(),
    no_retry            :: boolean(),
    default_validity    :: integer(), %% seconds
    max_validity        :: integer(), %% seconds
    features            :: [feature_dto()],
    credits             :: float()
}).

-record(auth_error_v1, {
    message :: binary()
}).

-record(auth_resp_v1, {
    req_id :: uuid_dto(),
    result :: #auth_customer_v1{} |
              #auth_error_v1{}
}).

%% ===================================================================
%% Sms Delivery Status
%% ===================================================================

-record(sms_delivery_status_req_v1, {
    req_id         :: uuid_dto(),
    customer_id    :: uuid_dto(),
    user_id        :: binary(),
    sms_request_id :: uuid_dto(),
    address        :: addr()
}).

-type common_sms_status() ::
    submitted                   |
    success_waiting_delivery    |
    success_no_delivery         |
    failure                     |
    enroute                     |
    delivered                   |
    expired                     |
    deleted                     |
    undeliverable               |
    accepted                    |
    unknown                     |
    rejected                    |
    unrecognized.

-record(sms_status_v1, {
    address   :: addr(),
    status    :: common_sms_status(),
    timestamp :: pos_integer()  %% utc unixtime
}).

-record(sms_delivery_status_resp_v1, {
    req_id   :: uuid_dto(),
    statuses :: [#sms_status_v1{}]
}).

%% ===================================================================
%% Authentication
%% ===================================================================

-type common_dto() ::
    %% authentication
    #auth_req_v1{}                       |
    #auth_resp_v1{}                      |

    %% sms delivery status
    #sms_delivery_status_req_v1{}        |
    #sms_delivery_status_resp_v1{}.

-endif. % common_dto_hrl
