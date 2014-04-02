-module(adto_funnel_tests).

-include("adto.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(gv(__Key, __PList),
    ((fun() ->
        proplists:get_value(__Key, __PList)
    end))()).

funnel_dto_test_() ->
    [?_test(success_auth_response()),
    ?_test(error_auth_response()),
    ?_test(started_event()),
    ?_test(stopped_event()),
    ?_test(client_online_event()),
    ?_test(client_offline_event()),
    ?_test(incoming_sms()),
    ?_test(delivery_receipt()),
    ?_test(ack()),
    ?_test(connections_req()),
    ?_test(connections_resp())].

%% ===================================================================
%% Auth Request
%% ===================================================================

auth_req_test_() ->

%% smpp types test
    TypeTestPList =
    [{connection_id, uuid:generate()},
    {ip, <<"127.0.0.1">>},
    {customer_id, <<"test_id">>},
    {user_id, <<"user_id">>},
    {password, <<"password">>},
    {is_cached, true}],
    ValidTypes = [receiver, transceiver, transmitter],
    ValSmppTypesTestsPlists =
        [ ?_test(auth_req([{type, T} | TypeTestPList])) || T <- ValidTypes],

%% invalid smpp types test
    InvTypeTestPList =
    [{connection_id, uuid:generate()},
    {ip, <<"127.0.0.1">>},
    {customer_id, <<"test_id">>},
    {user_id, <<"user_id">>},
    {password, <<"password">>},
    {is_cached, true}],
    InvalidTypes = [k1api, eoneapi, oneapi, 1, "list", <<"bin">>],
    InvSmppTypesTestsPlists =
        [ ?_assertError({badmatch, _}, auth_req([{type, T} | InvTypeTestPList])) || T <- InvalidTypes],

%% All tests
    InvSmppTypesTestsPlists ++
    ValSmppTypesTestsPlists.

auth_req(PList) ->
    DTO = #funnel_auth_request_dto{
        connection_id = ?gv(connection_id, PList),
        ip = ?gv(ip, PList),
        customer_id = ?gv(customer_id, PList),
        user_id = ?gv(customer_id, PList),
        password = ?gv(password, PList),
        type = ?gv(type, PList),
        is_cached = ?gv(is_cached, PList),
        timestamp = #fun_precise_time_dto{time = <<"120827114232">>, milliseconds = 1},
        expiration = #fun_precise_time_dto{time = <<"120827114232">>, milliseconds = 1}
    },
    {ok, Bin} = adto:encode(DTO),
    {ok, DTO} = adto:decode(#funnel_auth_request_dto{}, Bin).

%% ===================================================================
%% Auth Response
%% ===================================================================

success_auth_response() ->
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
    CustomerDTO = #funnel_auth_response_customer_dto{
        id = <<"system-id">>,
        uuid = uuid:generate(),
        priority = 0,
        rps = 100,
        allowed_sources = [#addr{addr = <<"375259090909">>, ton = 1, npi = 1}],
        default_source = #addr{addr = <<"375259090909">>, ton = 1, npi = 1},
        networks = [Network],
        providers = [Provider],
        default_provider_id = uuid:generate(),
        receipts_allowed = true,
        no_retry = true,
        default_validity = <<"000003000000000R">>,
        max_validity = 1234567,
        pay_type = prepaid
    },
    DTO = #funnel_auth_response_dto{
        connection_id = uuid:generate(),
        result = {customer, CustomerDTO}
    },
    {ok, Bin} = adto:encode(DTO),
    {ok, DTO} = adto:decode(#funnel_auth_response_dto{}, Bin).

error_auth_response() ->
    DTO = #funnel_auth_response_dto{
        connection_id = uuid:generate(),
        result = {error, "test error"}
    },
    {ok, Bin} = adto:encode(DTO),
    {ok, DTO} = adto:decode(#funnel_auth_response_dto{}, Bin).

%% ===================================================================
%% Funnel Events Test
%% ===================================================================

started_event() ->
    DTO = #funnel_started_event_dto{
        timestamp = <<"120827114232">>
    },
    {ok, Bin} = adto:encode(DTO),
    {ok, DTO} = adto:decode(#funnel_started_event_dto{}, Bin).

stopped_event() ->
    DTO = #funnel_stopped_event_dto{
        timestamp = <<"120827114232">>
    },
    {ok, Bin} = adto:encode(DTO),
    {ok, DTO} = adto:decode(#funnel_stopped_event_dto{}, Bin).

client_online_event() ->
    DTO = #funnel_client_online_event_dto{
        connection_id = uuid:generate(),
        customer_id = <<"system_id">>,
        user_id = <<"user_id">>,
        type = transmitter,
        connected_at = <<"120827114232">>,
        timestamp = <<"120827114232">>
    },
    {ok, Bin} = adto:encode(DTO),
    {ok, DTO} = adto:decode(#funnel_client_online_event_dto{}, Bin).

client_offline_event() ->
    DTO = #funnel_client_offline_event_dto{
        connection_id = uuid:generate(),
        customer_id = <<"system_id">>,
        user_id = <<"user_id">>,
        type = transmitter,
        connected_at = <<"120827114232">>,
        msgs_received = 1,
        msgs_sent = 1,
        errors = [#error_dto{error_code = 1, timestamp = <<"120827114232">>}],
        reason = normal,
        timestamp = <<"120827114232">>
    },
    {ok, Bin} = adto:encode(DTO),
    {ok, DTO} = adto:decode(#funnel_client_offline_event_dto{}, Bin).

%% ===================================================================
%% Funnel Incoming Sms Test
%% ===================================================================

valid_incoming_sms_encodings_test_() ->
    ValidEncodings = [gsm0338, ucs2, 5],
    [?_test(incoming_sms(E)) || E <- ValidEncodings].

incoming_sms() ->
    incoming_sms(gsm0338).
incoming_sms(Encoding) ->
    MessageDTO = #funnel_incoming_sms_message_dto{
        source = #addr{addr = <<"375259090909">>, ton = 1, npi = 1},
        dest = #addr{addr = <<"375259090909">>, ton = 1, npi = 1},
        message = <<"message">>,
        data_coding = Encoding
    },
    DTO = #funnel_incoming_sms_dto{
        id = uuid:generate(),
        messages = [MessageDTO]
    },
    {ok, Bin} = adto:encode(DTO),
    {ok, DTO} = adto:decode(#funnel_incoming_sms_dto{}, Bin).

%% ===================================================================
%% Funnel Delivery Receipt Test
%% ===================================================================

delivery_receipt() ->
    ReceiptDTO = #funnel_delivery_receipt_container_dto{
        message_id = <<"614">>,
        submit_date = <<"120827114232">>,
        done_date = <<"120827114232">>,
        message_state = delivered,
        source = #addr{addr = <<"375259090909">>, ton = 1, npi = 1},
        dest = #addr{addr = <<"375259090909">>, ton = 1, npi = 1}
    },
    DTO = #funnel_delivery_receipt_dto{
        id = uuid:generate(),
        receipts = [ReceiptDTO]
    },
    {ok, Bin} = adto:encode(DTO),
    {ok, DTO} = adto:decode(#funnel_delivery_receipt_dto{}, Bin).

%% ===================================================================
%% Funnel Ack Test
%% ===================================================================

ack() ->
    DTO = #funnel_ack_dto{
        id = uuid:generate()
    },
    {ok, Bin} = adto:encode(DTO),
    {ok, DTO} = adto:decode(#funnel_ack_dto{}, Bin).

%% ===================================================================
%% Funnel Connections
%% ===================================================================

connections_req() ->
    DTO = #funnel_connections_request_dto{
    },
    {ok, Bin} = adto:encode(DTO),
    {ok, DTO} = adto:decode(#funnel_connections_request_dto{}, Bin).

connections_resp() ->
    Error = #error_dto{error_code = 1, timestamp = <<"120827114232">>},
    Connection = #funnel_connection_dto{
        connection_id = uuid:generate(),
        remote_ip = <<"127.0.0.1">>,
        customer_id = <<"system-id">>,
        user_id = <<"user">>,
        connected_at = <<"120827114232">>,
        type = transmitter,
        msgs_received = 1,
        msgs_sent = 2,
        errors = [Error]
    },
    DTO = #funnel_connections_response_dto{
        connections = [Connection]
    },
    {ok, Bin} = adto:encode(DTO),
    {ok, DTO} = adto:decode(#funnel_connections_response_dto{}, Bin).
