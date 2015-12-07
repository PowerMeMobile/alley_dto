-module(adto_common_tests).

-spec test() -> ignore.

-include("adto.hrl").
-include_lib("eunit/include/eunit.hrl").

-spec common_dto_test_() -> ignore.
common_dto_test_()-> [
    ?_test(auth_req_v4()),
    ?_test(ok_auth_resp_v4()),
    ?_test(error_auth_resp_v4()),

    ?_test(sms_status_req()),
    ?_test(sms_status_resp()),

    ?_test(coverage_req()),
    ?_test(coverage_resp()),

    ?_test(blacklist_req()),
    ?_test(blacklist_resp()),

    ?_test(credit_req()),
    ?_test(credit_resp())
].

%% ===================================================================
%% Auth v4
%% ===================================================================

%% NOTE: there is not necessary to set fields with test values,
%% because of field validation is not implemented

auth_req_v4() ->
    DTO = #auth_req_v4{},
    ?assertEqual(DTO, decode(#auth_req_v4{}, encode(DTO))).

ok_auth_resp_v4() ->
    DTO = #auth_resp_v4{
        req_id = uuid:generate(),
        result = #auth_customer_v4{}
    },
    ?assertEqual(DTO, decode(#auth_resp_v4{}, encode(DTO))).

error_auth_resp_v4() ->
    DTO = #auth_resp_v4{
        req_id = uuid:generate(),
        result = #auth_error_v4{}
    },
    ?assertEqual(DTO, decode(#auth_resp_v4{}, encode(DTO))).

%% ===================================================================
%% Sms Status
%% ===================================================================

sms_status_req() ->
    DTO = #sms_status_req_v1{
        req_id = uuid:generate(),
        customer_uuid = uuid:generate(),
        user_id = <<"user">>,
        sms_req_id = uuid:generate()
    },
    ?assertEqual(DTO, decode(#sms_status_req_v1{}, encode(DTO))).

%% ===================================================================
%% Sms Status Resp Tests
%% ===================================================================

statuses() ->
    [
        <<"pending">>,
        <<"submitted">>,
        <<"failed">>,
        <<"sent">>,                     %% deprecated
        <<"success_waiting_delivery">>, %% deprecated
        <<"success_no_delivery">>,      %% deprecated
        <<"failure">>,                  %% deprecated
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

sms_status_resp() ->
    StatusesDTO = [
        #sms_status_v1{
            address = #addr{addr = <<"375269090909">>, ton = 1, npi = 1},
            status = Status,
            timestamp = 1405598755
        } || Status <- statuses()
    ],
    DTO = #sms_status_resp_v1{
        req_id = uuid:generate(),
        statuses = StatusesDTO
    },
    ?assertEqual(DTO, decode(#sms_status_resp_v1{}, encode(DTO))).

%% ===================================================================
%% Kelly API
%% ===================================================================

coverage_req() ->
    DTO = #coverage_req_v1{
        req_id = uuid:generate(),
        customer_id = <<"0">>
    },
    ?assertEqual(DTO, decode(#coverage_req_v1{}, encode(DTO))).

coverage_resp() ->
    ProviderId = uuid:generate(),
    Network = #network_v1{
        id = uuid:generate(),
        country_code = <<"375">>,
        number_len = 12,
        prefixes = [<<"33">>, <<"44">>],
        provider_id = ProviderId,
        is_home = false,
        sms_points = 2.0,
        sms_mult_points = 1.0
    },
    Provider = #provider_v1{
        id = ProviderId,
        gateway_id = uuid:generate(),
        bulk_gateway_id = uuid:generate(),
        receipts_supported = true,
        sms_add_points = 0.0
    },
    DTO = #coverage_resp_v1{
        req_id = uuid:generate(),
        networks = [Network],
        providers = [Provider],
        default_provider_id = ProviderId
    },
    ?assertEqual(DTO, decode(#coverage_resp_v1{}, encode(DTO))).

blacklist_req() ->
    DTO = #blacklist_req_v1{
        req_id = uuid:generate()
    },
    ?assertEqual(DTO, decode(#blacklist_req_v1{}, encode(DTO))).

blacklist_resp() ->
    Entry1 = #blacklist_entry_v1{
        id = uuid:generate(),
        dst_addr = #addr{addr = <<"375291112233">>, ton = 1, npi = 1},
        src_addr = #addr{addr = <<"Hello">>, ton = 5, npi = 0}
    },
    Entry2 = #blacklist_entry_v1{
        id = uuid:generate(),
        dst_addr = #addr{addr = <<"375291112233">>, ton = 1, npi = 1},
        src_addr = undefined
    },
    DTO = #blacklist_resp_v1{
        req_id = uuid:generate(),
        entries = [Entry1, Entry2]
    },
    ?assertEqual(DTO, decode(#blacklist_resp_v1{}, encode(DTO))).

credit_req() ->
    DTO = #credit_req_v1{
        req_id = uuid:generate(),
        customer_id = <<"0">>,
        credit = 100.0
    },
    ?assertEqual(DTO, decode(#credit_req_v1{}, encode(DTO))).

credit_resp() ->
    DTO = #credit_resp_v1{
        req_id = uuid:generate(),
        result = allowed,
        credit_left = 10.0
    },
    ?assertEqual(DTO, decode(#credit_resp_v1{}, encode(DTO))).

%% ===================================================================
%% Internals
%% ===================================================================

encode(DTO) ->
    {ok, Bin} = adto:encode(DTO),
    Bin.

decode(Type, Bin) ->
    {ok, DTO} = adto:decode(Type, Bin),
    DTO.
