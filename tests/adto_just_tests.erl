-module(adto_just_tests).

-include("adto.hrl").
-include("JustAsn.hrl").
-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% Variables
%% ===================================================================

str_uuid() -> "12fd794d-9e32-4cf6-b421-b797196b60e3".

sms_req_params() ->
	[{'Param',"registered_delivery",{boolean,true}},{'Param',"service_type",{string,[]}},{'Param',"no_retry",{boolean,false}},{'Param',"validity_period",{string,"000003000000000R"}},{'Param',"priority_flag",{integer,0}},{'Param',"esm_class",{integer,3}},{'Param',"protocol_id",{integer,0}}].

full_addr() ->
	{'FullAddr',"375296662323",1,1}.

%% ===================================================================
%% Just Sms Request Tests
%% ===================================================================

just_sms_request_test() ->
	DTO = #just_sms_request_dto{
		id = <<18,253,121,77,158,50,76,246,180,33,183,151,25,107,96,227>>,
		gateway_id = <<18,253,121,77,158,50,76,246,180,33,183,151,25,107,96,227>>,
		customer_id = <<18,253,121,77,158,50,76,246,180,33,183,151,25,107,96,227>>,
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
		id = adto_uuid:newid(),
		gateway_id = adto_uuid:newid(),
		customer_id = adto_uuid:newid(),
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
		gateway_id = adto_uuid:newid(),
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

%% %% ===================================================================
%% %% Just Delivery Receipt Tests
%% %% ===================================================================

%% just_delivery_receipt_encode_test() ->
%% 	erlang:error(not_implemented).

%% just_delivery_receipt_decode_test() ->
%% 	erlang:error(error).

