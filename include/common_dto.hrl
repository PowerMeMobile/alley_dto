-ifndef(common_dto_hrl).
-define(common_dto_hrl, included).

-include("adto_types.hrl").

-type utc_timestamp() :: os:timestamp().
-type utc_unix_time() :: pos_integer().

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
    req_id        :: uuid(),
    gateway_id    :: uuid(),
    customer_uuid :: uuid(),
    user_id       :: binary(),
    interface     :: client_type(),
    req_time      :: utc_timestamp(),
    def_time      :: undefined | utc_timestamp(),
    src_addr      :: addr(),
    type          :: regular,
    message       :: binary(),
    encoding      :: sms_req_enc(),
    params        :: plist(),
    dst_addrs     :: [addr()],
    msg_ids       :: [binary()],
    messages      :: undefined | [binary()],
    net_ids       :: [uuid()],
    prices        :: [float()]
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

-record(error_resp_v1, {
    req_id :: uuid(),
    error  :: term()
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

-record(auth_credentials, {
    customer_id :: binary(),
    user_id     :: binary(),
    password    :: binary()
}).

-record(auth_email, {
    email :: binary()
}).

-record(auth_msisdn, {
    msisdn :: #addr{}
}).

-record(auth_req_v2, {
    req_id    :: uuid(),
    auth_data :: #auth_credentials{}
               | #auth_email{}
               | #auth_msisdn{},
    interface :: atom()
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

-record(auth_customer_v2, {
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
    features            :: [#feature_v1{}],
    priority            :: integer(),
    rps                 :: integer()
}).

-record(auth_error_v1, {
    code    :: term(),
    %% deprecated, use `code'
    message :: binary()
}).

-record(auth_error_v2, {
    code :: term()
}).

-record(auth_resp_v1, {
    req_id :: uuid(),
    result :: #auth_customer_v1{} |
              #auth_error_v1{}
}).

-record(auth_resp_v2, {
    req_id :: uuid(),
    result :: #auth_customer_v2{} |
              #auth_error_v2{}
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
    timestamp :: utc_unix_time() %% change to utc_timestamp() in v2
}).

-record(sms_status_resp_v1, {
    req_id   :: uuid(),
    statuses :: [#sms_status_v1{}]
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
%% Inbox
%% ===================================================================

-record(inbox_req_v1, {
    req_id        :: uuid(),
    customer_uuid :: uuid(),
    user_id       :: binary(),
    operation     :: get_info
                   | list_all | list_new
                   | fetch_all | fetch_new | fetch_id
                   | delete_all | delete_read | delete_id,
    msg_ids       :: [binary()]
}).

-record(inbox_info_v1, {
    new   :: non_neg_integer(),
    total :: non_neg_integer()
}).

-record(inbox_msg_info_v1, {
    msg_id    :: uuid(),
    src_addr  :: addr(),
    dst_addr  :: addr(),
    size      :: non_neg_integer(),
    body      :: undefined | binary(),
    rcv_time  :: utc_timestamp(),
    state     :: all | new | read
}).

-record(inbox_resp_v1, {
    req_id :: uuid(),
    result :: {info, #inbox_info_v1{}}
            | {messages, [#inbox_msg_info_v1{}]}
            | {deleted, non_neg_integer()}
            | {error, term()}
}).

%% ===================================================================
%% Retrieve incoming
%% ===================================================================

-record(retrieve_incoming_req_v1, {
    req_id        :: uuid(),
    customer_uuid :: uuid(),
    user_id       :: binary(),
    dst_addr      :: addr(),
    batch_size    :: undefined | integer()
}).

-record(retrieve_incoming_resp_v1, {
    req_id   :: uuid(),
    messages :: [#inbox_msg_info_v1{}],
    pending  :: integer()
}).

%% ===================================================================
%% Just specific
%% ===================================================================

-record(block_req_v1, {
    req_id :: uuid(),
    sms_req_id  :: uuid()
}).

-record(block_resp_v1, {
    req_id :: uuid(),
    result :: ok | {error, term()}
}).

-record(unblock_req_v1, {
    req_id :: uuid(),
    sms_req_id :: uuid()
}).

-record(unblock_resp_v1, {
    req_id :: uuid(),
    result :: ok | {error, term()}
}).

-record(gateway_states_req_v1, {
    req_id :: uuid()
}).

-record(connection_state_v1, {
    id          :: integer(),
    host        :: binary(),
    port        :: 0..65535,
    bind_type   :: smpp_type(),
    system_id   :: binary(),
    password    :: binary(),
    system_type :: binary(),
    state       :: connected | connecting
}).

-record(gateway_state_v1, {
    id          :: binary(),
    name        :: binary(),
    host        :: binary(),
    state       :: started | stopped,
    connections :: [#connection_state_v1{}]
}).

-record(gateway_states_resp_v1, {
    req_id :: uuid(),
    result :: [#gateway_state_v1{}] | {error, term()}
}).

-record(start_gateway_req_v1, {
    req_id :: uuid(),
    gateway_id :: uuid()
}).

-record(start_gateway_resp_v1, {
    req_id :: uuid(),
    result :: ok | {error, term()}
}).

-record(stop_gateway_req_v1, {
    req_id :: uuid(),
    gateway_id :: uuid()
}).

-record(stop_gateway_resp_v1, {
    req_id :: uuid(),
    result :: ok | {error, term()}
}).

%% ===================================================================
%% Funnel specific
%% ===================================================================

-record(connections_req_v1, {
    req_id :: uuid()
}).

-record(connection_error_v1, {
    error_code :: pos_integer(),
    timestamp  :: utc_timestamp()
}).

-record(connection_v1, {
    connection_id :: uuid(),
    remote_ip     :: inet:ip4_address(),
    customer_id   :: binary(),
    user_id       :: binary(),
    connected_at  :: utc_timestamp(),
    bind_type     :: smpp_type(),
    msgs_received :: non_neg_integer(),
    msgs_sent     :: non_neg_integer(),
    errors        :: [#connection_error_v1{}]
}).

-record(connections_resp_v1, {
    req_id :: uuid(),
    connections :: [#connection_v1{}]
}).

-record(disconnect_req_v1, {
    req_id :: uuid(),
    customer_id :: binary(),
    user_id :: binary(),
    bind_type :: smpp_type() | [smpp_type()],
    connection_id :: uuid() | [uuid()]
}).

-record(disconnect_resp_v1, {
    req_id :: uuid()
}).

-record(throughput_req_v1, {
    req_id :: uuid()
}).

-record(throughput_counter_v1, {
    connection_id :: uuid(),
    direction :: in | out,
    count :: non_neg_integer()
}).

-record(throughput_slice_v1, {
    period_start :: utc_timestamp(),
    counters :: [#throughput_counter_v1{}]
}).

-record(throughput_resp_v1, {
    req_id :: uuid(),
    slices :: [#throughput_slice_v1{}]
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

    %% credit
    #credit_req_v1{}            |
    #credit_resp_v1{}           |

    %% blacklist
    #blacklist_req_v1{}         |
    #blacklist_resp_v1{}        |

    %% coverage
    #coverage_req_v1{}          |
    #coverage_resp_v1{}         |

    %% inbox
    #inbox_req_v1{}             |
    #inbox_resp_v1{}            |

    %% retrieve incoming
    #retrieve_incoming_req_v1{}  |
    #retrieve_incoming_resp_v1{} |

    %% just specific
    #block_req_v1{}             |
    #block_resp_v1{}            |
    #unblock_req_v1{}           |
    #unblock_resp_v1{}          |

    %% funnel specific
    #connections_req_v1{}       |
    #connections_resp_v1{}      |
    #disconnect_req_v1{}        |
    #disconnect_resp_v1{}       |
    #throughput_req_v1{}        |
    #throughput_resp_v1{}
    .

-endif. % common_dto_hrl
