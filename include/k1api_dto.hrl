-ifndef(k1api_dto_hrl).
-define(k1api_dto_hrl, included).

-include("adto_types.hrl").

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
    #k1api_process_inbox_request_dto{}              |
    #k1api_process_inbox_response_dto{}.

-endif. % k1api_dto_hrl
