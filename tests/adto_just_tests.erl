-module(adto_just_tests).

-include("adto.hrl").
-include("JustAsn.hrl").
-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% Just Sms Request Tests
%% ===================================================================

just_sms_request_test() ->
	DTO = #just_sms_request_dto{
		id = <<18,253,121,77,158,50,76,246,180,33,183,151,25,107,96,227>>,
		gateway_id = <<18,253,121,77,158,50,76,246,180,33,183,151,25,107,96,227>>,
		customer_id = <<18,253,121,77,158,50,76,246,180,33,183,151,25,107,96,227>>,
		client_type = k1api,
		type = regular,
		message = <<"message">>,
		encoding = {text, default},
		params = [#just_sms_request_param_dto{name = <<"registered_delivery">>, value = {boolean, true}}],
		source_addr = #addr_dto{addr = <<"375296662323">>, ton = 1, npi = 1},
		dest_addrs = {regular, [#addr_dto{addr = <<"375253723886">>, ton = 1, npi = 1}]},
		message_ids = [<<"634">>]
	},
	{ok, Bin} = adto:encode(DTO),
	{ok, DTO} = adto:decode(#just_sms_request_dto{}, Bin).

%% ===================================================================
%% Sms Response Tests
%% ===================================================================

just_sms_response_test() ->
	StatusDTO = #just_sms_status_dto{
		original_id = <<"614">>,
		dest_addr = #addr_dto{addr = <<"375296662323">>, ton = 1, npi = 1},
		status = success,
		parts_total = 1,
		part_index = undefined,
		message_id = <<"614">>,
		error_code = undefined
	},
	DTO = #just_sms_response_dto{
		id = uuid:newid(),
		gateway_id = uuid:newid(),
		customer_id = uuid:newid(),
		client_type = k1api,
		statuses = [StatusDTO],
		timestamp = <<"120827114305">>
	},
	{ok, Bin} = adto:encode(DTO),
	{ok, DTO} = adto:decode(#just_sms_response_dto{}, Bin).

%% ===================================================================
%% Just Incoming Sms Tests
%% ===================================================================

just_incoming_sms_test() ->
	DTO = #just_incoming_sms_dto{
		gateway_id = uuid:newid(),
		source = #addr_dto{addr = <<"375296662323">>, ton = 1, npi = 1},
		dest = #addr_dto{addr = <<"375296662323">>, ton = 1, npi = 1},
		message = <<"message">>,
		data_coding = 0,
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

just_delivery_receipt_test() ->
	ReceiptDTO = #just_receipt_dto{
		message_id = <<"614">>,
		message_state = delivered,
		source = #addr_dto{addr = <<"375296662323">>, ton = 1, npi = 1}
	},
	DTO = #just_delivery_receipt_dto{
		gateway_id = uuid:newid(),
		receipts = [ReceiptDTO],
		timestamp = 1346067785681000
	},
	{ok, Bin} = adto:encode(DTO),
	{ok, DTO} = adto:decode(#just_delivery_receipt_dto{}, Bin).
