-module(adto_k1api_tests).

-spec test() -> ignore.

-include("adto.hrl").
-include_lib("eunit/include/eunit.hrl").

-spec k1api_dto_test_() -> ignore.
k1api_dto_test_()-> [
    ?_test(remove_retrieved_sms_request()),
    ?_test(bad_type_encode()),
    ?_test(bad_type_decode()),

    ?_test(subscribe_incoming_sms_request()),
    ?_test(subscribe_incoming_sms_response()),
    ?_test(unsubscribe_incoming_sms_request()),
    ?_test(unsubscribe_incoming_sms_response()),

    ?_test(subscribe_sms_receipts_request()),
    ?_test(subscribe_sms_receipts_response()),
    ?_test(unsubscribe_sms_receipts_request()),
    ?_test(unsubscribe_sms_receipts_response()),
    ?_test(sms_delivery_receipt_notification()),

    ?_test(incoming_sms_request()),

    ?_test(process_inbox_request()),
    ?_test(process_inbox_response_messages()),
    ?_test(process_inbox_response_deleted()),
    ?_test(process_inbox_response_error())
].

%% ===================================================================
%% k1api Subscribe Sms Receipts Request
%% ===================================================================

subscribe_sms_receipts_request() ->
    DTO = #k1api_subscribe_sms_receipts_request_dto{
        id = uuid:generate(),
        customer_id = uuid:generate(),
        user_id = <<"user">>,
        url = <<"url">>,
        dest_addr = #addr{addr = <<"123456">>, ton = 1, npi = 1},
        callback_data = <<"callback">>
    },
    {ok, Bin} = adto:encode(DTO),
    {ok, DTO} = adto:decode(#k1api_subscribe_sms_receipts_request_dto{}, Bin).

%% ===================================================================
%% k1api Subscribe Sms Receipts Response
%% ===================================================================

subscribe_sms_receipts_response() ->
    DTO = #k1api_subscribe_sms_receipts_response_dto{
        id = uuid:generate()
    },
    {ok, Bin} = adto:encode(DTO),
    {ok, DTO} = adto:decode(#k1api_subscribe_sms_receipts_response_dto{}, Bin).

%% ===================================================================
%% k1api Unsubscribe Sms Receipts Request
%% ===================================================================

unsubscribe_sms_receipts_request() ->
    DTO = #k1api_unsubscribe_sms_receipts_request_dto{
        id = uuid:generate(),
        customer_id = uuid:generate(),
        user_id = <<"user">>,
        subscription_id = uuid:generate()
    },
    {ok, Bin} = adto:encode(DTO),
    {ok, DTO} = adto:decode(#k1api_unsubscribe_sms_receipts_request_dto{}, Bin).

%% ===================================================================
%% k1api Unsubscribe Sms Receipts Response
%% ===================================================================

unsubscribe_sms_receipts_response() ->
    DTO = #k1api_unsubscribe_sms_receipts_response_dto{
        id = uuid:generate()
    },
    {ok, Bin} = adto:encode(DTO),
    {ok, DTO} = adto:decode(#k1api_unsubscribe_sms_receipts_response_dto{}, Bin).

%% ===================================================================
%% k1api Sms Delivery Status
%% ===================================================================

sms_delivery_receipt_notification() ->
    DTO = #k1api_sms_delivery_receipt_notification_dto{
        id = uuid:generate(),
        dest_addr = #addr{addr = <<"123456">>, ton = 1, npi = 1},
        status = <<"submitted">>,
        callback_data = <<"callback">>,
        url = <<"url">>
    },
    {ok, Bin} = adto:encode(DTO),
    {ok, DTO} = adto:decode(#k1api_sms_delivery_receipt_notification_dto{}, Bin).

%% ===================================================================
%% Remove Retrieved Sms Request Tests
%% ===================================================================

remove_retrieved_sms_request() ->
    DTO = #k1api_remove_retrieved_sms_request_dto{
        id = uuid:generate(),
        message_ids = [<<"123">>, <<"456">>]
    },
    {ok, Bin} = adto:encode(DTO),
    {ok, DTO} = adto:decode(#k1api_remove_retrieved_sms_request_dto{}, Bin).

%% ===================================================================
%% Subscribe Incoming Sms Request
%% ===================================================================

subscribe_incoming_sms_request() ->
    DTO = #k1api_subscribe_incoming_sms_request_dto{
        id = uuid:generate(),
        customer_id = uuid:generate(),
        user_id = <<"user">>,
        dest_addr = #addr{addr = <<"123456">>, ton = 1, npi = 1},
        notify_url = <<"some_url">>,
        criteria = <<"criteria">>,
        notification_format = undefined,
        correlator = <<"correlator">>,
        callback_data = <<"callback">>
    },
    {ok, Bin} = adto:encode(DTO),
    {ok, DTO} = adto:decode(#k1api_subscribe_incoming_sms_request_dto{}, Bin).

%% ===================================================================
%% Subscribe Incoming Sms Response
%% ===================================================================

subscribe_incoming_sms_response() ->
    DTO = #k1api_subscribe_incoming_sms_response_dto{
        id = uuid:generate(),
        subscription_id = uuid:generate()
    },
    {ok, Bin} = adto:encode(DTO),
    {ok, DTO} = adto:decode(#k1api_subscribe_incoming_sms_response_dto{}, Bin).

unsubscribe_incoming_sms_request() ->
    DTO = #k1api_unsubscribe_incoming_sms_request_dto{
        id = uuid:generate(),
        customer_id = uuid:generate(),
        user_id = <<"user">>,
        subscription_id = uuid:generate()
    },
    {ok, Bin} = adto:encode(DTO),
    {ok, DTO} = adto:decode(#k1api_unsubscribe_incoming_sms_request_dto{}, Bin).

unsubscribe_incoming_sms_response() ->
    DTO = #k1api_unsubscribe_incoming_sms_response_dto{
        id = uuid:generate()
    },
    {ok, Bin} = adto:encode(DTO),
    {ok, DTO} = adto:decode(#k1api_unsubscribe_incoming_sms_response_dto{}, Bin).

incoming_sms_request() ->
    DTO = #k1api_sms_notification_request_dto{
        callback_data = <<"callback">>,
        datetime = {1355,224026, 0},
        dest_addr = #addr{addr = <<"123456">>, ton = 1, npi = 1},
        message_id = <<"123">>,
        message = <<"message">>,
        sender_addr = #addr{addr = <<"123456">>, ton = 1, npi = 1},
        notify_url  = <<"notify_url">>
    },
    {ok, Bin} = adto:encode(DTO),
    {ok, DTO} = adto:decode(#k1api_sms_notification_request_dto{}, Bin).

%% ===================================================================
%% Kelly API
%% ===================================================================

process_inbox_request() ->
    DTO = #k1api_process_inbox_request_dto{
        id = uuid:generate(),
        customer_id = <<"0">>,
        user_id = <<"user">>,
        operation = fetch_id,
        message_ids = [uuid:generate(), uuid:generate()]
    },
    {ok, Bin} = adto:encode(DTO),
    {ok, DTO} = adto:decode(#k1api_process_inbox_request_dto{}, Bin).

process_inbox_response_messages() ->
    Msg = #k1api_process_inbox_response_message_dto{
        id = uuid:generate(),
        new = true,
        from = #addr{addr = <<"1234567890">>, ton = 1, npi = 1},
        to = #addr{addr = <<"0987654321">>, ton = 1, npi = 1},
        timestamp = {1355,224026, 0},
        size = 5,
        text = <<"Hello">>
    },
    DTO = #k1api_process_inbox_response_dto{
        id = uuid:generate(),
        result = {messages, [Msg]}
    },
    {ok, Bin} = adto:encode(DTO),
    {ok, DTO} = adto:decode(#k1api_process_inbox_response_dto{}, Bin).

process_inbox_response_deleted() ->
    DTO = #k1api_process_inbox_response_dto{
        id = uuid:generate(),
        result = {deleted, 10}
    },
    {ok, Bin} = adto:encode(DTO),
    {ok, DTO} = adto:decode(#k1api_process_inbox_response_dto{}, Bin).

process_inbox_response_error() ->
    DTO = #k1api_process_inbox_response_dto{
        id = uuid:generate(),
        result = {error, <<"Fix me!">>}
    },
    {ok, Bin} = adto:encode(DTO),
    {ok, DTO} = adto:decode(#k1api_process_inbox_response_dto{}, Bin).

%% ===================================================================
%% Bad Type Request
%% ===================================================================

bad_type_encode() ->
    BadDTO = unsupported_type,
    ?assertError({k1api_encode_not_supported,unsupported_type} , adto_k1api:encode(BadDTO)).

bad_type_decode() ->
    BadDTO = unsupported_type,
    ?assertError({k1api_decode_not_supported,unsupported_type} , adto_k1api:decode(BadDTO, <<>>)).
