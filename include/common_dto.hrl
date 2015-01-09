-ifndef(common_dto_hrl).
-define(common_dto_hrl, included).

-include("adto_types.hrl").

%% ===================================================================
%% Authentication
%% ===================================================================

-record(auth_req_v1, {
    req_id          :: uuid(),
    customer_id     :: binary(),
    user_id         :: binary(),
    password        :: binary(),
    connection_type :: binary()
}).

-record(auth_customer_v1, {
    customer_uuid       :: uuid(),
    customer_id         :: binary(),
    user_id             :: binary(),
    pay_type            :: pay_type(),
    credit              :: float(),
    allowed_sources     :: [addr()],
    default_source      :: addr() | undefined,
    networks            :: [network_dto()],
    providers           :: [provider_dto()],
    default_provider_id :: uuid() | undefined,
    receipts_allowed    :: boolean(),
    no_retry            :: boolean(),
    default_validity    :: integer(), %% seconds
    max_validity        :: integer(), %% seconds
    features            :: [feature_dto()]
}).

-record(auth_error_v1, {
    code    :: term(), %% reserved for future extension
    message :: binary()
}).

-record(auth_resp_v1, {
    req_id :: uuid(),
    result :: #auth_customer_v1{} |
              #auth_error_v1{}
}).

%% ===================================================================
%% Sms Status
%% ===================================================================

-record(sms_status_req_v1, {
    req_id      :: uuid(),
    customer_id :: uuid(),
    user_id     :: binary(),
    sms_req_id  :: uuid()
}).

-type sms_status() ::
    submitted     |
    failure       |
    enroute       |
    delivered     |
    expired       |
    deleted       |
    undeliverable |
    accepted      |
    unknown       |
    rejected      |
    unrecognized.

-record(sms_status_v1, {
    address   :: addr(),
    status    :: sms_status(),
    timestamp :: pos_integer()  %% utc unixtime
}).

-record(sms_status_resp_v1, {
    req_id   :: uuid(),
    statuses :: [#sms_status_v1{}]
}).

%% ===================================================================
%% Authentication
%% ===================================================================

-type common_dto() ::
    %% authentication
    #auth_req_v1{}              |
    #auth_resp_v1{}             |

    %% sms delivery status
    #sms_status_req_v1{}        |
    #sms_status_resp_v1{}.

-endif. % common_dto_hrl
