-module(adto_just_tests).

-include("adto.hrl").
-include_lib("eunit/include/eunit.hrl").

just_dto_test_() ->
	[?_test(just_sms_response()),
	?_test(just_incoming_sms()),
	?_test(just_delivery_receipt())].

%% ===================================================================
%% Just Sms Request Tests
%% ===================================================================

sms_request_encodings_test_() ->
	ValidEncodings = [default, gsm0338, ascii, latin1, ucs2, 5],
	[?_test(sms_request_by_encoding(E)) || E <- ValidEncodings].

sms_request_by_encoding(Encoding) ->
	DTO = #just_sms_request_dto{
		id = <<18,253,121,77,158,50,76,246,180,33,183,151,25,107,96,227>>,
		gateway_id = <<18,253,121,77,158,50,76,246,180,33,183,151,25,107,96,227>>,
		customer_id = <<18,253,121,77,158,50,76,246,180,33,183,151,25,107,96,227>>,
		user_id = <<"user">>,
		client_type = k1api,
		type = regular,
		message = <<"message">>,
		encoding = Encoding,
		params = [#just_sms_request_param_dto{name = <<"registered_delivery">>, value = {boolean, true}}],
		source_addr = #addr{addr = <<"375296662323">>, ton = 1, npi = 1},
		dest_addrs = {regular, [#addr{addr = <<"375253723886">>, ton = 1, npi = 1}]},
		message_ids = [<<"634">>]
	},
	{ok, Bin} = adto:encode(DTO),
	{ok, DTO} = adto:decode(#just_sms_request_dto{}, Bin).

%% ===================================================================
%% Sms Response Tests
%% ===================================================================

just_sms_response() ->
	StatusDTO = #just_sms_status_dto{
		original_id = <<"614">>,
		dest_addr = #addr{addr = <<"375296662323">>, ton = 1, npi = 1},
		status = success,
		parts_total = 1,
		part_index = undefined,
		message_id = <<"614">>,
		error_code = undefined
	},
	DTO = #just_sms_response_dto{
		id = uuid:generate(),
		gateway_id = uuid:generate(),
		customer_id = uuid:generate(),
		client_type = k1api,
		statuses = [StatusDTO],
		timestamp = <<"120827114305">>
	},
	{ok, Bin} = adto:encode(DTO),
	{ok, DTO} = adto:decode(#just_sms_response_dto{}, Bin).

%% ===================================================================
%% Just Incoming Sms Tests
%% ===================================================================

just_incoming_sms() ->
	DTO = #just_incoming_sms_dto{
		gateway_id = uuid:generate(),
		source = #addr{addr = <<"375296662323">>, ton = 1, npi = 1},
		dest = #addr{addr = <<"375296662323">>, ton = 1, npi = 1},
		message = <<"message">>,
		data_coding = gsm0338,
		parts_ref_num = undefined,
		parts_count = undefined,
		part_index = undefined,
		timestamp = <<"120827114305">>
	},
	{ok, Bin} = adto:encode(DTO),
	{ok, DTO} = adto:decode(#just_incoming_sms_dto{}, Bin).

%% ===================================================================
%% Just Delivery Receipt Tests
%% ===================================================================

just_delivery_receipt() ->
	ReceiptDTO = #just_receipt_dto{
		message_id = <<"614">>,
		message_state = delivered,
		source = #addr{addr = <<"375296662323">>, ton = 1, npi = 1}
	},
	DTO = #just_delivery_receipt_dto{
		gateway_id = uuid:generate(),
		receipts = [ReceiptDTO],
		timestamp = <<"120827114305">>
	},
	{ok, Bin} = adto:encode(DTO),
	{ok, DTO} = adto:decode(#just_delivery_receipt_dto{}, Bin).
