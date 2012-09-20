-module(adto_k1api_tests).

-include("adto.hrl").
-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% k1api Sms Delivery Status Request Tests
%% ===================================================================

k1api_sms_delivery_status_request_test() ->
	DTO = #k1api_sms_delivery_status_request_dto{
		id = uuid:newid(),
		customer_id = uuid:newid(),
		user_id = <<"user">>,
		sms_request_id = uuid:newid(),
		address = #addr_dto{addr = <<"375269090909">>, ton = 1, npi = 1}
	},
	{ok, Bin} = adto:encode(DTO),
	{ok, DTO} = adto:decode(#k1api_sms_delivery_status_request_dto{}, Bin).

%% ===================================================================
%% k1api Sms Delivery Status Response Tests
%% ===================================================================

k1api_sms_delivery_status_response_test() ->
	StatusDTO = #k1api_sms_status_dto{
		address = #addr_dto{addr = <<"375269090909">>, ton = 1, npi = 1},
		status = delivered
	},
	DTO = #k1api_sms_delivery_status_response_dto{
		id = uuid:newid(),
		statuses = [StatusDTO]
	},
	{ok, Bin} = adto:encode(DTO),
	io:format("Bin"),
	{ok, DTO} = adto:decode(#k1api_sms_delivery_status_response_dto{}, Bin).
