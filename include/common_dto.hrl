-ifndef(common_dto_hrl).
-define(common_dto_hrl, included).

-include("adto_types.hrl").

%% ===================================================================
%% Sms Req
%% ===================================================================

-type sms_req_enc() ::
    default     |
    gsm0338     |
    ascii       |
    latin1      |
    ucs2        |
    integer().

-type plist() :: [{atom(), term()}].

-record(sms_req_v1, {
    req_id          :: uuid(),
    gateway_id      :: uuid(),
    customer_id     :: uuid(),
    user_id         :: binary(),
    interface       :: client_type(),
    src_addr        :: addr(),
    type            :: regular | part,

    %reg_dlr         :: non_neg_integer(),
    %esm_class       :: non_neg_integer(),
    %validify        :: binary(),

    %% ?
    encodings       :: [sms_req_enc()],

    dst_addrs       :: [addr()],
    in_msg_ids      :: [binary()],
    messages        :: [binary()],
    paramss         :: [plist()],
    net_ids         :: [uuid()],
    prices          :: [float()]
}).

%% ===================================================================
%% Shared
%% ===================================================================

-record(network_v1, {
    id              :: uuid(),
    country_code    :: binary(),
    number_len      :: pos_integer(),
    prefixes        :: [binary()],
    provider_id     :: uuid(),
    is_home         :: boolean(),
    sms_points      :: float(),
    sms_mult_points :: float(),

    name            :: binary(),
    country         :: binary(),
    gmt_diff        :: binary(),
    dst             :: binary()
}).

-record(provider_v1, {
    id                 :: uuid(),
    gateway_id         :: uuid(),
    bulk_gateway_id    :: uuid(),
    receipts_supported :: boolean(),
    sms_add_points     :: float()
}).

-record(feature_v1, {
    name  :: binary(),
    value :: binary()
}).

%% ===================================================================
%% Authentication
%% ===================================================================

-record(auth_req_v1, {
    req_id      :: uuid(),
    customer_id :: binary(),
    user_id     :: binary(),
    password    :: binary(),
    interface   :: atom()
}).

-record(auth_customer_v1, {
    customer_uuid       :: uuid(),
    customer_id         :: binary(),
    user_id             :: binary(),
    pay_type            :: pay_type(),
    credit              :: float(),
    allowed_sources     :: [addr()],
    default_source      :: addr() | undefined,
    networks            :: [#network_v1{}],
    providers           :: [#provider_v1{}],
    default_provider_id :: uuid() | undefined,
    receipts_allowed    :: boolean(),
    no_retry            :: boolean(),
    default_validity    :: integer(), %% seconds
    max_validity        :: integer(), %% seconds
    features            :: [#feature_v1{}]
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
%% Retrieve sms
%% ===================================================================

-record(retrieve_sms_req_v1, {
    req_id      :: uuid(),
    customer_id :: uuid(),
    user_id     :: binary(),
    dst_addr    :: addr(),
    batch_size  :: undefined | integer()
}).

-record(msg_info_v1, {
    msg_id    :: binary(),
    src_addr  :: addr(),
    body      :: binary(),
    recv_time :: calendar:datetime()
}).

-record(retrieve_sms_resp_v1, {
    req_id   :: uuid(),
    messages :: [#msg_info_v1{}],
    pending  :: integer()
}).

%% ===================================================================
%% Credit
%% ===================================================================

-record(credit_req_v1, {
    req_id      :: uuid(),
    customer_id :: binary(),
    credit      :: float()
}).

-record(credit_resp_v1, {
    req_id      :: uuid(),
    result      :: allowed | denied,
    credit_left :: float()
}).

%% ===================================================================
%% Blacklist
%% ===================================================================

-record(blacklist_req_v1, {
    req_id :: uuid()
}).

-record(blacklist_entry_v1, {
    id       :: uuid(),
    dst_addr :: addr(),
    src_addr :: undefined | addr()
}).

-record(blacklist_resp_v1, {
    req_id  :: uuid(),
    entries :: [#blacklist_entry_v1{}]
}).

%% ===================================================================
%% Coverage
%% ===================================================================

-record(coverage_req_v1, {
    req_id      :: uuid(),
    customer_id :: binary()
}).

-record(coverage_resp_v1, {
    req_id              :: uuid(),
    networks            :: [#network_v1{}],
    providers           :: [#provider_v1{}],
    default_provider_id :: uuid() | undefined
}).

%% ===================================================================
%% Types
%% ===================================================================

-type common_dto() ::
    %% sms request
    #sms_req_v1{}               |

    %% authentication
    #auth_req_v1{}              |
    #auth_resp_v1{}             |

    %% sms status
    #sms_status_req_v1{}        |
    #sms_status_resp_v1{}       |

    %% retrieve sms
    #retrieve_sms_req_v1{}      |
    #retrieve_sms_resp_v1{}     |

    %% credit
    #credit_req_v1{}            |
    #credit_resp_v1{}           |

    %% blacklist
    #blacklist_req_v1{}         |
    #blacklist_resp_v1{}        |

    %% coverage
    #coverage_req_v1{}          |
    #coverage_resp_v1{}.

-endif. % common_dto_hrl
