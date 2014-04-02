-module(adto_k1api_tests).

-spec test() -> ignore.

-include("adto.hrl").
-include_lib("eunit/include/eunit.hrl").

-spec k1api_dto_test_() -> ignore.
k1api_dto_test_()->
    [?_test(auth_request()),
    ?_test(auth_customer_response()),
    ?_test(auth_error_response()),
    ?_test(sms_delivery_status_request()),
    ?_test(sms_delivery_status_response()),
    ?_test(retrieve_sms_request()),
    ?_test(retrieve_sms_request2()),
    ?_test(retrieve_sms_response()),
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

    ?_test(incoming_sms_request())].

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
%% k1api Auth Request
%% ===================================================================

auth_request() ->
    DTO = #k1api_auth_request_dto{
        id = uuid:generate(),
        customer_id = <<"test-sys-id">>,
        user_id = <<"user">>,
        password = <<"password">>
    },
    {ok, Bin} = adto:encode(DTO),
    {ok, DTO} = adto:decode(#k1api_auth_request_dto{}, Bin).


%% ===================================================================
%% k1api Auth Request
%% ===================================================================

auth_customer_response() ->
    Provider = #provider_dto{
        id = uuid:generate(),
        gateway = uuid:generate(),
        bulk_gateway = uuid:generate(),
        receipts_supported = true
    },
    Network = #network_dto{
        id = uuid:generate(),
        country_code = <<"375">>,
        numbers_len = 12,
        prefixes = [<<"33">>, <<"44">>],
        provider_id = uuid:generate()
    },
    Customer = #k1api_auth_response_customer_dto{
        id = <<"system-id">>,
        uuid = uuid:generate(),
        pay_type = prepaid, %% postpaid
        allowed_sources = [#addr{addr = <<"375259090909">>, ton = 1, npi = 1}],
        default_source = #addr{addr = <<"375259090909">>, ton = 1, npi = 1},
        networks = [Network],
        providers = [Provider],
        default_provider_id = uuid:generate(),
        receipts_allowed = true,
        no_retry = true,
        default_validity = 12345,
        max_validity = 1234567
    },
    DTO = #k1api_auth_response_dto{
        id = uuid:generate(),
        result = {customer, Customer}
    },
    {ok, Bin} = adto:encode(DTO),
    {ok, DTO} = adto:decode(#k1api_auth_response_dto{}, Bin).

auth_error_response() ->
    DTO = #k1api_auth_response_dto{
        id = uuid:generate(),
        result = {error, <<"Unknown customer">>}
    },
    {ok, Bin} = adto:encode(DTO),
    {ok, DTO} = adto:decode(#k1api_auth_response_dto{}, Bin).

%% ===================================================================
%% Sms Delivery Status Request Tests
%% ===================================================================

sms_delivery_status_request() ->
    DTO = #k1api_sms_delivery_status_request_dto{
        id = uuid:generate(),
        customer_id = uuid:generate(),
        user_id = <<"user">>,
        sms_request_id = uuid:generate(),
        address = #addr{addr = <<"375269090909">>, ton = 1, npi = 1}
    },
    ?assertEqual(DTO, decode(#k1api_sms_delivery_status_request_dto{}, encode(DTO))).

%% ===================================================================
%% Sms Delivery Status Response Tests
%% ===================================================================

statuses() ->
    [
        <<"submitted">>,
        <<"success_waiting_delivery">>,
        <<"success_no_delivery">>,
        <<"failure">>,
        <<"enroute">>,
        <<"delivered">>,
        <<"expired">>,
        <<"deleted">>,
        <<"undeliverable">>,
        <<"accepted">>,
        <<"unknown">>,
        <<"rejected">>,
        <<"unrecognized">>
    ].

sms_delivery_status_response() ->
    StatusesDTO = [#k1api_sms_status_dto{
        address = #addr{addr = <<"375269090909">>, ton = 1, npi = 1},
        status = Status
    } || Status <- statuses()],
    DTO = #k1api_sms_delivery_status_response_dto{
        id = uuid:generate(),
        statuses = StatusesDTO
    },
    {ok, Bin} = adto:encode(DTO),
    {ok, DTO} = adto:decode(#k1api_sms_delivery_status_response_dto{}, Bin).

%% ===================================================================
%% Retrieve Sms Request Tests
%% ===================================================================

%% test with defined optional parameter batch_size
retrieve_sms_request() ->
    DTO = #k1api_retrieve_sms_request_dto{
        id = uuid:generate(),
        customer_id = uuid:generate(),
        user_id = <<"user">>,
        dest_addr = #addr{addr = <<"375269090909">>, ton = 1, npi = 1},
        batch_size = 5
    },
    {ok, Bin} = adto:encode(DTO),
    {ok, DTO} = adto:decode(#k1api_retrieve_sms_request_dto{}, Bin).

%% test with UNdefined optional parameter batch_size
retrieve_sms_request2() ->
    DTO = #k1api_retrieve_sms_request_dto{
        id = uuid:generate(),
        customer_id = uuid:generate(),
        user_id = <<"user">>,
        dest_addr = #addr{addr = <<"375269090909">>, ton = 1, npi = 1},
        batch_size = undefined
    },
    {ok, Bin} = adto:encode(DTO),
    {ok, DTO} = adto:decode(#k1api_retrieve_sms_request_dto{}, Bin).


%% ===================================================================
%% Retrieve Sms Response Tests
%% ===================================================================

retrieve_sms_response() ->
    MessageDTO = #k1api_retrieved_sms_dto{
        datetime = {1355,224026, 0},
        sender_addr = #addr{addr = <<"375269090909">>, ton = 1, npi = 1},
        message_id = <<"123">>,
        message = <<"message">>
    },
    DTO = #k1api_retrieve_sms_response_dto{
        id = uuid:generate(),
        messages = [MessageDTO],
        total = 5
    },
    {ok, Bin} = adto:encode(DTO),
    {ok, DTO} = adto:decode(#k1api_retrieve_sms_response_dto{}, Bin).


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
%% Bad Type Request
%% ===================================================================

bad_type_encode() ->
    BadDTO = unsupported_type,
    ?assertError({k1api_encode_not_supported,unsupported_type} , adto_k1api:encode(BadDTO)).

bad_type_decode() ->
    BadDTO = unsupported_type,
    ?assertError({k1api_decode_not_supported,unsupported_type} , adto_k1api:decode(BadDTO, <<>>)).

%% ===================================================================
%% Internals
%% ===================================================================

encode(DTO) ->
    {ok, Bin} = adto:encode(DTO),
    Bin.

decode(Type, Bin) ->
    {ok, DTO} = adto:decode(Type, Bin),
    DTO.
