-ifndef(k1api_dto_hrl).
-define(k1api_dto_hrl, included).

-include("adto_types.hrl").

%% ===================================================================
%% k1api Sms Delivery Status Request
%% ===================================================================

-record(k1api_sms_delivery_status_request_dto, {
    id              :: uuid(),
    customer_id     :: uuid(),
    user_id         :: binary(),
    sms_request_id  :: uuid(),
    address         :: addr()
}).

%% ===================================================================
%% k1api Sms Delivery Status Response
%% ===================================================================

-type k1api_sms_status() ::
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

-record(k1api_sms_status_dto, {
    address     :: addr(),
    status      :: k1api_sms_status(),
    timestamp   :: pos_integer()  %% utc unixtime
}).

-record(k1api_sms_delivery_status_response_dto, {
    id          :: uuid(),
    statuses    :: [#k1api_sms_status_dto{}]
}).

%% ===================================================================
%% k1api Retrieve Sms Request
%% ===================================================================

-record(k1api_retrieve_sms_request_dto, {
    id          :: uuid(),
    customer_id :: uuid(),
    user_id     :: binary(), %% <<"user_id">>
    dest_addr   :: addr(),
    batch_size  :: undefined | integer()
}).

%% ===================================================================
%% k1api Retrieve Sms Response
%% ===================================================================

-record(k1api_retrieved_sms_dto, {
    datetime    :: pos_integer(), %% utc unixtime
    sender_addr :: addr(),
    message_id  :: binary(), %% <<"123">>
    message     :: binary() %% <<"message">>
}).

-record(k1api_retrieve_sms_response_dto, {
    id          :: uuid(),
    messages    :: [#k1api_retrieved_sms_dto{}],
    total       :: integer()
}).

%% ===================================================================
%% k1api Remove Retrieved Sms Request
%% ===================================================================

-record(k1api_remove_retrieved_sms_request_dto, {
    id          :: uuid(),
    message_ids :: [binary()] %% [<<"123">>]
}).

%% ===================================================================
%% k1api Subscribe Incoming Sms
%% ===================================================================

-record(k1api_subscribe_incoming_sms_request_dto, {
    id                  :: uuid(),
    customer_id         :: uuid(),
    user_id             :: binary(),
    dest_addr           :: addr(),
    notify_url          :: binary(),
    criteria            :: undefined | binary(),
    notification_format :: undefined | binary(), %% <<"json">>
    correlator          :: undefined | binary(),
    callback_data       :: undefined | binary()
}).

-record(k1api_subscribe_incoming_sms_response_dto, {
    id                  :: uuid(),
    subscription_id     :: uuid()
}).

%% ===================================================================
%% k1api Unsubscribe Incoming Sms
%% ===================================================================

-record(k1api_unsubscribe_incoming_sms_request_dto, {
    id                  :: uuid(),
    customer_id         :: uuid(),
    user_id             :: binary(),
    subscription_id     :: uuid()
}).

-record(k1api_unsubscribe_incoming_sms_response_dto, {
    id                  :: uuid()
}).

%% ===================================================================
%% k1api Incoming Sms Notification
%% ===================================================================

-record(k1api_sms_notification_request_dto, {
    callback_data       :: binary(),
    datetime            :: pos_integer(), %% utc unixtime
    dest_addr           :: addr(),
    message_id          :: binary(),
    message             :: binary(),
    sender_addr         :: addr(),
    notify_url          :: binary()
}).

%% ===================================================================
%% k1api Auth
%% ===================================================================

-record(k1api_auth_request_dto, {
    id                  :: uuid(),
    customer_id         :: binary(), %% <<"system_id">>
    user_id             :: binary(),
    password            :: binary(),
    connection_type     :: binary()
}).

-record(k1api_auth_response_customer_dto, {
    id                  :: binary(), %% customer id
    uuid                :: uuid(),
    pay_type            :: pay_type(),
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

-type k1api_auth_response_result() ::
    {customer, #k1api_auth_response_customer_dto{}} |
    {error, binary()}.

-record(k1api_auth_response_dto, {
    id     :: uuid(),
    result :: k1api_auth_response_result()
}).

%% ===================================================================
%% k1api Sms Receipts Subscriptions
%% ===================================================================

-record(k1api_subscribe_sms_receipts_request_dto, {
    id              :: uuid(),
    customer_id     :: uuid(),
    user_id         :: binary(), %% <<"user">>
    url             :: binary(),
    dest_addr       :: addr(),
    callback_data   :: binary() %% <<"callback">>
}).

-record(k1api_subscribe_sms_receipts_response_dto, {
    id              :: uuid()
}).

-record(k1api_unsubscribe_sms_receipts_request_dto, {
    id              :: uuid(),
    customer_id     :: uuid(),
    user_id         :: binary(),
    subscription_id :: uuid()
}).

-record(k1api_unsubscribe_sms_receipts_response_dto, {
    id              :: uuid()
}).

-record(k1api_sms_delivery_receipt_notification_dto, {
    id              :: uuid(),
    dest_addr       :: addr(),
    status          :: k1api_sms_status(),
    callback_data   :: binary(),
    url             :: binary()
}).

%% ===================================================================
%% k1api Kelly API
%% ===================================================================

-record(k1api_coverage_request_dto, {
    id              :: uuid(),
    customer_id     :: binary(),
    user_id         :: binary(),
    version         :: binary()
}).

-record(k1api_coverage_response_dto, {
    id              :: uuid(),
    networks        :: [network_dto()],
    providers       :: [provider_dto()],
    default_provider_id :: uuid() | undefined
}).

-record(k1api_blacklist_request_dto, {
    id              :: uuid(),
    customer_id     :: binary(),
    user_id         :: binary(),
    version         :: binary()
}).

-record(k1api_blacklist_response_dto, {
    id              :: uuid(),
    entries         :: [blacklist_entry_dto()]
}).

-record(k1api_request_credit_request_dto, {
    id              :: uuid(),
    customer_id     :: binary(),
    credit          :: float()
}).

-record(k1api_request_credit_response_dto, {
    id              :: uuid(),
    result          :: allowed | denied,
    credit_left     :: float()
}).

-record(k1api_process_inbox_request_dto, {
    id              :: uuid(),
    customer_id     :: binary(),
    user_id         :: binary(),
    operation       :: list_all | list_new
                     | fetch_all | fetch_new | fetch_id
                     | kill_all | kill_old | kill_id,
    message_ids     :: [binary()]
}).

-record(k1api_process_inbox_response_message_dto, {
    id                  :: uuid(),
    new                 :: boolean(),
    from                :: addr(),
    to                  :: addr(),
    timestamp           :: pos_integer(),
    size                :: non_neg_integer(),
    text                :: undefined | binary()
}).

-type k1api_process_inbox_response_result() ::
    {messages, [#k1api_process_inbox_response_message_dto{}]} |
    {deleted, non_neg_integer()} |
    {error, binary()}.

-record(k1api_process_inbox_response_dto, {
    id              :: uuid(),
    result          :: k1api_process_inbox_response_result()
}).

-type k1api_dto() ::
    %% k1api auth
    #k1api_auth_request_dto{}                       |
    #k1api_auth_response_dto{}                      |

    %% k1api sms delivery status
    #k1api_sms_delivery_status_request_dto{}        |
    #k1api_sms_delivery_status_response_dto{}       |

    %% k1api retrieve sms
    #k1api_retrieve_sms_request_dto{}               |
    #k1api_retrieve_sms_response_dto{}              |
    #k1api_remove_retrieved_sms_request_dto{}       |

    %% k1api incoming sms subscription
    #k1api_subscribe_incoming_sms_request_dto{}     |
    #k1api_subscribe_incoming_sms_response_dto{}    |
    #k1api_unsubscribe_incoming_sms_request_dto{}   |
    #k1api_unsubscribe_incoming_sms_response_dto{}  |
    #k1api_sms_notification_request_dto{}           |

    %% k1api delivery receipt subscription
    #k1api_subscribe_sms_receipts_request_dto{}     |
    #k1api_subscribe_sms_receipts_response_dto{}    |
    #k1api_unsubscribe_sms_receipts_request_dto{}   |
    #k1api_unsubscribe_sms_receipts_response_dto{}  |
    #k1api_sms_delivery_receipt_notification_dto{}  |

    %% k1api kelly api
    #k1api_coverage_request_dto{}                   |
    #k1api_coverage_response_dto{}                  |
    #k1api_blacklist_request_dto{}                  |
    #k1api_blacklist_response_dto{}                 |
    #k1api_request_credit_request_dto{}             |
    #k1api_request_credit_response_dto{}            |
    #k1api_process_inbox_request_dto{}              |
    #k1api_process_inbox_response_dto{}.

-endif. % k1api_dto_hrl
