-module(adto_funnel_tests).

-include("adto.hrl").
-include("FunnelAsn.hrl").
-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% Variables
%% ===================================================================

bin_uuid() -> <<138,192,76,106,232,107,75,217,141,162,249,70,128,211,111,2>>.

str_uuid() -> "1ad95af3-b33c-4eba-be89-a31ff0b14d39".

precise_time_dto() ->
	#precise_time_dto{
		time = utc_time(),
		milliseconds = 573
	}.
utc_time() ->
	"120823124151".

addr_dto() ->
	#addr_dto{
		addr = <<"addr">>,
		ton = 0,
		npi = 1
	}.

network_dto() ->
	#network_dto{
	  id = bin_uuid(),
	  country_code = <<"country_code">>,
	  numbers_len = 12,
	  prefixes = [<<"12">>, <<"34">>],
	  provider_id = bin_uuid()
	 }.

provider_dto() ->
	#provider_dto{
		id = bin_uuid(),
		gateway = bin_uuid(),
		bulk_gateway  = bin_uuid(),
		receipts_supported = true
	}.

%% ===================================================================
%% Funnel Auth Tests
%% ===================================================================

funnel_auth_request_test() ->
	DTO = #funnel_auth_request_dto{
		connection_id = uuid:newid(),
		ip = <<"127.0.0.1">>,
		customer_id = <<"test-sys-id">>,
		user_id = <<"user">>,
		password = <<"password">>,
		type = transmitter,
		is_cached = true,
		timestamp = #precise_time_dto{time = <<"120827114232">>, milliseconds = 1},
		expiration = #precise_time_dto{time = <<"120827114232">>, milliseconds = 1}
	},
	{ok, Bin} = adto:encode(DTO),
	{ok, DTO} = adto:decode(#funnel_auth_request_dto{}, Bin).

funnel_success_auth_response_test() ->
	Provider = #provider_dto{
		id = uuid:newid(),
		gateway = uuid:newid(),
		bulk_gateway = uuid:newid(),
		receipts_supported = true
	},
	Network = #network_dto{
		id = uuid:newid(),
		country_code = <<"375">>,
		numbers_len = 12,
		prefixes = [<<"33">>, <<"44">>],
		provider_id = uuid:newid()
	},
	CustomerDTO = #funnel_auth_response_customer_dto{
		id = <<"system-id">>,
		uuid = uuid:newid(),
		priority = 0,
		rps = 100,
		allowed_sources = [#addr_dto{addr = <<"375259090909">>, ton = 1, npi = 1}],
		default_source = #addr_dto{addr = <<"375259090909">>, ton = 1, npi = 1},
		networks = [Network],
		providers = [Provider],
		default_provider_id = uuid:newid(),
		receipts_allowed = true,
		no_retry = true,
		default_validity = <<"000003000000000R">>,
		max_validity = 1234567
	},
	DTO = #funnel_auth_response_dto{
		connection_id = uuid:newid(),
		result = {customer, CustomerDTO}
	},
	{ok, Bin} = adto:encode(DTO),
	{ok, DTO} = adto:decode(#funnel_auth_response_dto{}, Bin).

funnel_error_auth_response_test() ->
	DTO = #funnel_auth_response_dto{
		connection_id = uuid:newid(),
		result = {error, "test error"}
	},
	{ok, Bin} = adto:encode(DTO),
	{ok, DTO} = adto:decode(#funnel_auth_response_dto{}, Bin).

%% ===================================================================
%% Funnel Events Test
%% ===================================================================

funnel_started_event_test() ->
	DTO = #funnel_started_event_dto{
		timestamp = <<"120827114232">>
	},
	{ok, Bin} = adto:encode(DTO),
	{ok, DTO} = adto:decode(#funnel_started_event_dto{}, Bin).

funnel_stopped_event_test() ->
	DTO = #funnel_stopped_event_dto{
		timestamp = <<"120827114232">>
	},
	{ok, Bin} = adto:encode(DTO),
	{ok, DTO} = adto:decode(#funnel_stopped_event_dto{}, Bin).

funnel_client_online_event_test() ->
	DTO = #funnel_client_online_event_dto{
		connection_id = uuid:newid(),
		customer_id = <<"system_id">>,
		user_id = <<"user_id">>,
		type = transmitter,
		connected_at = <<"120827114232">>,
		timestamp = <<"120827114232">>
	},
	{ok, Bin} = adto:encode(DTO),
	{ok, DTO} = adto:decode(#funnel_client_online_event_dto{}, Bin).

funnel_client_offline_event_test() ->
	DTO = #funnel_client_offline_event_dto{
		connection_id = uuid:newid(),
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

funnel_incoming_sms_test() ->
	MessageDTO = #funnel_incoming_sms_message_dto{
		source = #addr_dto{addr = <<"375259090909">>, ton = 1, npi = 1},
		dest = #addr_dto{addr = <<"375259090909">>, ton = 1, npi = 1},
	 	message = <<"message">>,
		data_coding = {text, gsm0338}
	},
	DTO = #funnel_incoming_sms_dto{
		id = uuid:newid(),
		messages = [MessageDTO]
	},
	{ok, Bin} = adto:encode(DTO),
	{ok, DTO} = adto:decode(#funnel_incoming_sms_dto{}, Bin).

%% ===================================================================
%% Funnel Delivery Receipt Test
%% ===================================================================

funnel_delivery_receipt_test() ->
	ReceiptDTO = #funnel_delivery_receipt_container_dto{
		message_id = <<"614">>,
		submit_date = <<"20827114232">>,
		done_date = <<"20827114232">>,
		message_state = delivered,
		source = #addr_dto{addr = <<"375259090909">>, ton = 1, npi = 1},
		dest = #addr_dto{addr = <<"375259090909">>, ton = 1, npi = 1}
	},
	DTO = #funnel_delivery_receipt_dto{
		id = uuid:newid(),
		receipts = [ReceiptDTO]
	},
	{ok, Bin} = adto:encode(DTO),
	{ok, DTO} = adto:decode(#funnel_delivery_receipt_dto{}, Bin).

%% ===================================================================
%% Funnel Ack Test
%% ===================================================================

funnel_ack_test() ->
	DTO = #funnel_ack_dto{
		id = uuid:newid()
	},
	{ok, Bin} = adto:encode(DTO),
	{ok, DTO} = adto:decode(#funnel_ack_dto{}, Bin).
